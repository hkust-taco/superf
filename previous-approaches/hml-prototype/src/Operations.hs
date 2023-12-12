--------------------------------------------------------------------------
--  Common operations on types
--------------------------------------------------------------------------
module Operations(                  
                 -- * Generalization, skolemization and instantiation
                   generalize
                 , skolemize
                 , instantiate
                 , instantiateAnnot
                 , quantify
                 , xgeneralize
                 , normalize, constructedForm

                 -- * Creat fresh type variables
                 , freshSkolems, freshInstanceVar
                 , uniqueReset
                 
                 -- * Inference
                 , Infer, runInfer
                 , HasTypeVar
                 , freeTvs, freeSkolems, (|->), subst

                 -- * Helpers
                 , message
                 , check, failure
                 , onlyIf
                 , isTau, isFlexTau, isScheme, isUniVar, ground

                 , readRef, writeRef, newRef
                 , withRankInf, withRankIncrease, getRank
                 ) where

import Debug.Trace( trace )
import Data.IORef( IORef, newIORef, modifyIORef, readIORef, writeIORef )
import System.IO.Unsafe( unsafePerformIO )
import Data.List( sort )
import Control.Monad ( ap )
import PPrint
import Types
import Subst
import Gamma


--------------------------------------------------------------------------
-- Generalize
-- Uses efficient generalization by lambda-ranking unifiable variables. See:
-- See: George Kuan and David McQueen, "Efficient ML type inference with ranked type variables"
--------------------------------------------------------------------------
generalize :: Gamma -> Infer Type -> Infer Type
generalize gamma inf
  = do tp    <- withRankInf inf
       tvsTp <- freeTvs tp       
       (gtp,gtvs) <- xgeneralize tp 
       -- message $ "generalize over: " ++ show tp ++ ": " ++ show tvs
       assertGen tvsTp gtvs -- assert that we generalize correctly
       normalize gtp
  where
    assertGen tvsTp tvs
      = do -- assert that we generalize correctly
           tvsG  <- freeTvs (gammaCoDomain gamma)
           let tvsX = (tvsTp `diff` tvsG)
           if (sort tvs /= sort tvsX) 
            then message ("warning: different generalization:\n tvs:  " ++ show (sort tvs) ++ "\ntvsX: " ++ show (show tvsX))
            else return ()

-- | "quantify" is used when type schemes are unified
quantify :: Type -> Infer Type
quantify tp
  = do (gtp,_) <- xgeneralize tp
       return gtp

xgeneralize :: Type -> Infer (Type,[TypeVar])
xgeneralize tp
  = do tvsTp <- freeTvs tp       
       depth <- getRank
       tvs   <- genTvs depth tvsTp
       qs    <- freshQuantifiers tvs
       stp   <- subQuantifiers qs tvs tp
       -- gtp   <- mkForallQ qs stp
       return (mkForallQ qs stp,tvs)
  where
    genTvs depth []
      = return []
    genTvs depth (tv@(TypeVar _ (Uni ref)):tvs)
      = do bound <- readRef ref
           case bound of
             Instance tp rank  | rank > depth
                -> do tvsTp <- freeTvs tp                      
                      gtvs  <- genTvs depth (tvsTp `union` tvs)
                      return ([tv] `union` gtvs)
             _  -> genTvs depth tvs

subQuantifiers qs tvs tp
  = -- subNew (tvIds tvs) (map (\q -> TVar (TypeVar (quantId q) Quantified)) qs) |-> tp       
    do mapM_ (\(quant,TypeVar _ (Uni ref)) -> writeRef ref (Equal (TVar (TypeVar (quantId quant) Quantified)))) 
             (zip qs tvs)
       subst tp

filterM pred []
  = return []
filterM pred (x : xs)
  = do keep <- pred x
       xs'  <- filterM pred xs
       if keep then return (x:xs') else return xs'


--------------------------------------------------------------------------
-- Instantiation 
--------------------------------------------------------------------------

-- | Instantiate a type 
instantiate :: Type -> Infer Rho
instantiate tp
  = do t <- ground tp
       case t of
         Forall q tp
             -> do tv   <- freshTVar q
                   stp  <- subNew [quantId q] [tv] |-> tp
                   -- message $ "instantiate: " ++ show tp ++ ": " ++ show stp
                   instantiate stp
         rho -> return rho
            
-- | Instantiate the the "some" quantifiers of an annotation to fresh type variables
instantiateAnnot :: Annot -> Infer ([Type],Type)
instantiateAnnot (Annot [] tp)
  = do ntp <- normalize tp
       return ([],ntp)
instantiateAnnot (Annot qs tp)
  = do tvs  <- freshTVars qs
       stp  <- subNew (quantIds qs) tvs |-> tp
       ntp  <- normalize stp
       return (tvs,ntp)


--------------------------------------------------------------------------
-- Skolemization
--------------------------------------------------------------------------
-- | Skolemize a quantified type and return the newly introduced skolem variables
skolemize :: Type -> Infer ([TypeVar],Rho)
skolemize tp
  = do t <- ground tp
       case splitQuants t of
          (qs,rho) | not (null qs)
            -> do sks  <- freshSkolems (length qs)
                  srho <- subNew (quantIds qs) (map TVar sks) |-> rho
                  -- message ("skolemize: " ++ show tp ++ " to " ++ show srho)
                  return (sks, srho)
          _ -> return ([],tp)


---------------------------------------------------------------------------
-- constructed form
---------------------------------------------------------------------------
constructedForm tp
  = case tp of
      Forall (Quant id bound) rho
        -> do eq <- checkTVar id rho
              if eq then constructedForm rho
                    else return tp
      _ -> return tp
  where
    checkTVar id rho
      = case rho of 
         TVar (TypeVar id2 (Uni ref))
            -> do bound <- readRef ref
                  case bound of
                    Equal t  -> checkTVar id t
                    _        -> return False
         TVar (TypeVar id2 Quantified)  | id == id2
           -> return True
         _ -> return False
             

--------------------------------------------------------------------------
-- normalization
--------------------------------------------------------------------------
normalize :: Type -> Infer Type
normalize tp
  = do stp <- subst tp  -- apply all substitutions
       (tvs,ntp) <- normalizeSchemeFtv stp
       return ntp

normalizeSchemeFtv :: Type -> Infer ([TypeVar],Type)
normalizeSchemeFtv tp
  = case tp of
      Forall (Quant id bound) rho     
                  -> do (tvs,nrho) <- normalizeSchemeFtv rho
                        if (not (tvFromId id `elem` tvs))
                         then return (tvs,nrho)
                         else do (btvs,nbound) <- normalizeSchemeFtv bound
                                 case nrho of
                                   (TVar (TypeVar id2 Quantified)) | id == id2
                                      -> return (btvs,nbound)
                                   _  -> do tp <- if (isRho nbound)
                                                   then (subNew [id] [nbound]) |-> nrho
                                                   else return (Forall (Quant id nbound) nrho)
                                            return (btvs `union` (remove (tvFromId id) tvs), tp)
      _           -> do tvs <- ftv tp
                        return (tvs,tp)
      

isRho :: Type -> Bool
isRho tp
  = case tp of
      Forall _ _ -> False
      Bottom     -> False
      _          -> True

--------------------------------------------------------------------------
-- Free type variables
--------------------------------------------------------------------------
freeSkolems :: HasTypeVar a => a  -> Infer [TypeVar]
freeSkolems tp
  = do tvs <- ftv tp
       return [tv | tv <- tvs, isSkolem (tvFlavour tv)]


-- | return the free unifiable variables of a type
freeTvs :: HasTypeVar a => a -> Infer [TypeVar]
freeTvs tp
  = do tvs <- ftv tp
       return [tv | tv <- tvs, isUni (tvFlavour tv)]


--------------------------------------------------------------------------
-- Type variables
--------------------------------------------------------------------------

-- | Things that have type variables 
class HasTypeVar a where
  -- | Return the free type variables
  ftv   :: a -> Infer [TypeVar]

  -- | Apply a substitution 
  (|->) :: Sub -> a -> Infer a

  -- | substitute the free reference type variables
  subst :: a -> Infer a



instance HasTypeVar a => HasTypeVar [a] where
  ftv xs
    = do tvss <- mapM ftv xs
         return (foldl union [] tvss)

  sub |-> xs
    = mapM (sub |->) xs

  subst xs
    = mapM subst xs

instance HasTypeVar Type where
  ftv tp
    = case tp of
        Forall (Quant id bound) rho     
                          -> do tvs <- ftv rho
                                if (tvFromId id `elem` tvs)
                                 then do btvs <- ftv bound 
                                         return (btvs `union` (remove (tvFromId id) tvs))
                                 else return tvs
        TApp t1 t2        -> do tvs1 <- ftv t1
                                tvs2 <- ftv t2
                                return (tvs1 `union` tvs2)
        TVar tv           -> case tv of
                               TypeVar id (Uni ref)  
                                  -> do bound <- readRef ref
                                        case bound of
                                          Instance _ _ -> return [tv]
                                          Equal t      -> ftv t 
                               _  -> return [tv]
        TCon _            -> return []
        Bottom            -> return []
           

  sub |-> tp
    = case tp of
        Forall (Quant id bound) rho     
                          -> do srho <- (subRemove sub [id]) |-> rho
                                sbnd <- sub |-> bound
                                return (Forall (Quant id sbnd) srho)
        TApp t1 t2        -> do st1 <- sub |-> t1
                                st2 <- sub |-> t2
                                return (TApp st1 st2)
        TCon name         -> return tp
        TVar tv           -> case tv of
                               TypeVar id (Uni ref)  
                                  -> do bound <- readRef ref
                                        case bound of
                                          Equal t       -> sub |-> t
                                          Instance _ _  -> case subLookup sub tv of
                                                            Just newtp -> return newtp
                                                            Nothing    -> return tp
                               _  -> case subLookup sub tv of   -- replace even bound ones, useful for instantiation
                                       Just newtp -> return newtp
                                       Nothing    -> return tp
        Bottom            -> return tp

  subst tp
    = case tp of
        Forall (Quant id bound) rho   
                        -> do srho <- subst rho
                              sbnd <- subst bound
                              return (Forall (Quant id sbnd) srho)
        TApp tp1 tp2    -> do stp1 <- subst tp1
                              stp2 <- subst tp2
                              return (TApp stp1 stp2)
        TVar (TypeVar _ (Uni ref))
                        -> do bound <- readRef ref
                              case bound of
                                Equal t   -> do ft <- subst t
                                                writeRef ref (Equal ft)
                                                return ft
                                Instance _ _ -> return tp
        _               -> return tp
             

instance HasTypeVar Bound where
  ftv bound
    = case bound of
        Equal tp      -> error "Operations.ftv: equality quantifier?"
        Instance tp _ -> ftv tp

  sub |-> bound
    = case bound of
        Equal tp    -> error "Operations.|->: equality quantifier?"
        Instance tp r -> do stp <- sub |-> tp
                            return (Instance stp r)

  subst bound
    = case bound of
       Instance tp r -> do stp <- subst tp
                           return (Instance stp r)
       Equal tp     -> error "Operations.subst: equality quantifier?"



--------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------
check :: Bool -> String -> Infer ()
check pred msg
  = if pred then return () 
            else failure msg

failure :: String -> a
failure msg
  = error ("error: " ++ msg)

onlyIf :: Bool -> Infer () -> Infer ()
onlyIf pred inf
  = if pred then inf else return ()

--------------------------------------------------------------------------
-- Fresh type variables 
--------------------------------------------------------------------------
-- | return fresh skolem variables
freshSkolems :: Int -> Infer  [TypeVar]
freshSkolems
  = freshTypeVars Skolem

-- | return fresh bound variables
freshQuantifiers :: [TypeVar] -> Infer  [Quant]
freshQuantifiers tvs
  = mapM freshQuantifier tvs

freshQuantifier (TypeVar _ (Uni ref))
  = do bound <- readRef ref
       id    <- freshId
       case bound of
         Equal tp -> error "Operations.freshQuantifier: do subst?"
         Instance tp rank -> return (Quant id tp)

-- | return fresh unifiable types
freshTVars :: [Quant] -> Infer [Type]
freshTVars qs
  = mapM freshTVar qs

-- | return a fresh unifiable type 
freshTVar :: Quant -> Infer Type
freshTVar (Quant id tp)
  = freshInstanceVar tp 
      

freshInstanceVar :: Type -> Infer Type
freshInstanceVar tp 
  = do rank   <- getRank          -- instantiate under the current rank
       scheme <- isScheme tp
       ref <- newRef (if scheme then Instance tp rank else Equal tp)
       tv  <- freshTypeVar (Uni ref)
       return (TVar tv)


-- | return fresh type variables of a certain |Flavour|
freshTypeVars :: Flavour -> Int -> Infer  [TypeVar]
freshTypeVars fl n
  = mapM (\_ -> freshTypeVar fl) [1..n]

-- | return a fresh type variable
freshTypeVar :: Flavour -> Infer TypeVar
freshTypeVar fl
  = do id <- freshId
       return (TypeVar id fl)

-- | return a fresh identifier
freshId :: Infer Id
freshId 
  = do id <- unique
       return id


--------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------

-- | Is this a monotype?  
isTau tp
  = case tp of
      Forall _ _ -> return False
      TApp t1 t2 -> do b1 <- isTau t1
                       b2 <- isTau t2
                       return (b1 && b2)
      TVar (TypeVar _ (Uni ref))
        -> do bound <- readRef ref
              case bound of
                Equal t -> isTau t
                Instance Bottom _ -> return True
                Instance _ _      -> return False
      Bottom  -> return False
      TCon _  -> return True


-- | Is this a monotype with flexible bounds only?  
isFlexTau tp
  = case tp of
      Forall _ _ -> return False
      TApp t1 t2 -> do b1 <- isFlexTau t1
                       b2 <- isFlexTau t2
                       return (b1 && b2)
      TVar (TypeVar _ (Uni ref))
        -> do bound <- readRef ref
              case bound of
                Equal t -> isFlexTau t
                -- Instance Bottom _ -> return True
                Instance _ _      -> return True
      Bottom  -> return False
      TCon _  -> return True

-- | Is this a type scheme?  
isScheme tp
  = case tp of
      Forall q rho -> return True
      TVar (TypeVar _ (Uni ref))
        -> do bound <- readRef ref
              case bound of
                Equal t -> isScheme t
                Instance _ _  -> return False
      Bottom  -> return True
      _       -> return False

ground :: Type -> Infer Type
ground tp@(TVar (TypeVar _ (Uni ref)))
  = do bound <- readRef ref
       case bound of
         Equal t      -> ground t
         Instance _ _ -> return tp
ground tp
  = return tp


isUniVar :: Type -> Infer Bool
isUniVar tp
  = do t <- ground tp
       case t of
         TVar (TypeVar _ (Uni _)) 
            -> return True
         _  -> return False

--------------------------------------------------------------------------
-- Infer
--------------------------------------------------------------------------
-- | The type inference monad, just IO for convenience
newtype Infer a  = Infer (Env -> IO a)

type Env = Rank

runInfer :: Infer a -> IO a
runInfer (Infer inf)
  = inf 0

instance Functor Infer where
  fmap f (Infer inf)  = Infer (\env -> fmap f (inf env))

instance Monad Infer where
  return x          = Infer (\env -> return x)
  (Infer inf) >>= f = Infer (\env -> do x <- inf env
                                        case f x of
                                          Infer inf2 -> inf2 env)
instance Applicative Infer where
  pure  = return
  (<*>) = ap

message :: String -> Infer ()
message msg
  = liftIO $ putStrLn msg


-- Ranks
withRank :: Rank -> Infer a -> Infer a
withRank r (Infer inf)
  = Infer (\rank -> inf r)

getRank :: Infer Rank
getRank
  = Infer (\rank -> return rank)

withRankIncrease :: Infer a -> Infer a
withRankIncrease inf
  = do r <- getRank
       withRank (r+1) inf

withRankInf :: Infer a -> Infer a
withRankInf inf
  = withRank rankInf inf

-- Refs
liftIO :: IO a -> Infer a
liftIO io
  = Infer (\env -> io)

readRef :: IORef a -> Infer a
readRef ref
  = liftIO (readIORef ref)

writeRef :: IORef a -> a -> Infer ()
writeRef ref x
  = liftIO (writeIORef ref x)

newRef :: a -> Infer (IORef a)
newRef x
  = liftIO (newIORef x)


{-# NOINLINE uniqueInt #-}
uniqueInt :: IORef Int
uniqueInt = unsafePerformIO (newIORef 0)

-- | Quick and dirty unique numbers :-)
unique :: Infer Int
unique = do u <- readRef uniqueInt
            writeRef uniqueInt (u+1)
            return u

uniqueReset :: Infer ()
uniqueReset
  = writeRef uniqueInt 0


