--------------------------------------------------------------------------
--  Unification and matching
--------------------------------------------------------------------------
module Unify ( unify ) where

import Data.IORef
import PPrint
import Types
import Subst
import Operations


--------------------------------------------------------------------------
-- unification 
--------------------------------------------------------------------------

unify :: Type -> Type -> Infer ()
unify (TCon n1) (TCon n2)   | n1 == n2
  = return ()

unify (TVar v1) (TVar v2)   | v1 == v2
  = return ()

unify (TApp t1 t2) (TApp u1 u2) 
  = do unify t1 u1
       unify t2 u2

unify (TVar v1) t2    | isUni (tvFlavour v1)
  = unifyVar v1 t2

unify t1 (TVar v2)    | isUni (tvFlavour v2)
  = unifyVar v2 t1

-- this case assumes that types are in normal form
unify t1@(Forall _ _) t2@(Forall _ _)   
  = case (splitQuants t1, splitQuants t2) of
      ((qs1,r1), (qs2,r2))  | length qs1 == length qs2
         -> assert "Unify.Forall" (all isBottom (qs1++qs2)) $
            do let ids1 = quantIds qs1
                   ids2 = quantIds qs2
               sks <- freshSkolems (length ids1)
               rho1 <- subNew ids1 (map TVar sks) |-> r1
               rho2 <- subNew ids2 (map TVar sks) |-> r2
               unify rho1 rho2
               -- check for escaping skolems
               sk1 <- freeSkolems t1
               sk2 <- freeSkolems t2
               check (sks `disjoint` (sk1 `union` sk2)) 
                     ("type is not polymorphic enough in unify:\n type1: " ++ show (pretty t1) ++ "\n type2: " ++ show (pretty t2))                               
      _  -> failure ("cannot unify types:\n type1: " ++ show (pretty t1) ++ "\n type2: " ++ show (pretty t2))

unify t1 t2
  = failure ("cannot unify types:\n type1: " ++ show (pretty t1) ++ "\n type2: " ++ show (pretty t2))


isBottom (Quant _ Bottom) = True
isBottom _                = False

---------------------------------------------------------------------------
-- Unify variables
---------------------------------------------------------------------------

-- | Unify a variable
unifyVar tv@(TypeVar id1 (Uni ref1)) tp2
  = do bound1 <- readRef ref1
       case bound1 of
         Equal tp1 -> unify tp1 tp2
         Instance t1 rank1
           -> case tp2 of
               TVar (TypeVar id2 (Uni ref2)) 
                  -> do bound2 <- readRef ref2
                        case bound2 of
                          Equal t2  
                            -> unify (TVar tv) t2    -- note: we can't shorten here since tv could be an element of t2
                          Instance t2 rank2
                            -> do t <- unifyScheme t1 t2
                                  writeRef ref1 (Equal tp2)
                                  writeInstance ref2 t (min rank1 rank2)
               _  -> do tvs <- freeTvs tp2
                        check (not (tv `elem` tvs)) ("infinite type: " ++ show tv ++ " and " ++ show tp2)  -- occurs check
                        subsume tp2 t1
                        writeRef ref1 (Equal tp2)
                        -- adjust the lambda-rank of the unifiable variables in tp2
                        adjustRank rank1 tp2

writeInstance ref tp rank
  = do scheme <- isScheme tp
       if scheme then writeRef ref (Instance tp rank)
                 else writeRef ref (Equal tp)


-- | adjust the lambda-rank of the unifiable variables in a type (note: this can be combined with the occurrence check)
adjustRank :: Rank -> Type -> Infer ()
adjustRank rank tp
  = case tp of
      TVar (TypeVar id2 (Uni ref2))
                      -> do bound <- readRef ref2
                            case bound of
                              Equal t  -> adjustRank rank t
                              Instance t rank2 -> onlyIf (rank2 > rank) (writeRef ref2 (Instance t rank)) 
                              -- Instance _ rank2 -> error "Unify.adjustRank: adjusting a type scheme"
      Forall  q rho   -> assert "Unify.adjustRank.Forall: adjusting type scheme" (isBottom q) $
                         adjustRank rank rho   
      TApp t1 t2      -> do adjustRank rank t1
                            adjustRank rank t2
      _               -> return ()
                                

---------------------------------------------------------------------------
-- Subsume
---------------------------------------------------------------------------
subsume :: Type -> Scheme -> Infer ()
subsume tp1 scheme
  = do tp2 <- constructedForm scheme
       case tp2 of
         Bottom 
            -> return ()
         _  -> do (sks,rho1) <- skolemize tp1
                  rho2       <- instantiate tp2
                  unify rho1 rho2
                  -- check for escaping skolems
                  sk1 <- freeSkolems tp1
                  sk2 <- freeSkolems tp2
                  check (sks `disjoint` (sk1 `union` sk2))  
                        ("type is not polymorphic enough in subsume:\n type1: " ++ show tp1 ++ "\n type2: " ++ show tp2)
        

---------------------------------------------------------------------------
-- Unify two type schemes (ie. find their join)
---------------------------------------------------------------------------
unifyScheme :: Scheme -> Scheme -> Infer Scheme
unifyScheme s1 s2
  = do tp1 <- constructedForm s1
       tp2 <- constructedForm s2
       case (tp1,tp2) of
         (Bottom,_)  -> return tp2
         (_,Bottom)  -> return tp1
         _ -> do rho <- withRankIncrease $  -- Instantiate under an increased rank so we can quantify afterwards
                          do rho1 <- instantiate tp1
                             rho2 <- instantiate tp2
                             unify rho1 rho2       
                             return rho1
                 quantify rho


