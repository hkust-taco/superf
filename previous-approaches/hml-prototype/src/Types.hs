--------------------------------------------------------------------------
-- Basic definition of types and terms
--------------------------------------------------------------------------
module Types( 
            -- * Definitions
              Name, Id
            , Term(..)
            , Annot(..)
            , Type(..), Rho, Tau, Scheme, FType
            , Bound(..), Quant(..)

            -- * Terms
            , isAnnot

            -- * Type variables
            , TypeVar(..), Flavour(..)
            , tvId, tvIds, tvFlavour, tvFromId
            , quantId, quantIds
            , isUni, isQuantified, isSkolem
            , Rank, rankInf
            , splitQuants
            
            
            -- * Type operations

            , mkForall, mkForallQ, mkAnnot, mkFun, mkTuple, mkList
            , annotAny
            , splitFun
            
            -- * misc
            , union, disjoint, intersect, diff, subseteq, remove
            , assert, assertM

            -- * Features
            , Feature(..)

            ) where

import Data.List( partition )
import PPrint
import Data.IORef( IORef, readIORef )
import System.IO.Unsafe( unsafePerformIO )
--------------------------------------------------------------------------
-- Type inference features
--------------------------------------------------------------------------
data Feature  = SupportRigidAnnotations     -- do not instantiate or generalize type annotations
              | SupportPropagation          -- propagate types through lambda expressions and let-bindings
              | SupportPropagateToArg       -- propagate parameter types to argument expressions
              deriving Eq


--------------------------------------------------------------------------
-- Terms
--------------------------------------------------------------------------
type Name = String      

data Term = Var Name              -- x
          | Lit Int               -- 3
          | App Term Term         -- f x
          | Lam Name Term         -- \x -> x
          | ALam Name Annot Term  -- \(x::Int) -> x
          | Let Name Term Term    -- let x = f y in x+1
          | Ann Term Annot        -- (f x) :: Int
          | Rigid Term            -- experimental: rigid x  (do not instantiate/generalize the type of "x")


-- | is an expression annotated?
isAnnot :: Term -> Bool
isAnnot (Rigid expr)          = True
isAnnot (Ann expr ann)        = True
isAnnot (Let name expr body)  = isAnnot body
isAnnot _                     = False


--------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------

type Id    = Int    -- Identifiers

type Scheme = Type   -- type schemes with flexible quantifiers
type FType  = Type   -- system-F types
type Rho    = Type   -- Unquantified F-type
type Tau    = Type   -- No ForAlls anywhere

-- | Type annotations: "some a. type". A type annotation is closed under "some" type variables
data Annot = Annot [Quant] Type    

-- | Types
data Type  = Forall Quant Type     -- ^ "forall (a >= tp). tp"
           | TVar TypeVar          -- ^ "a"
           | TCon Name             -- ^ "Int"    
           | TApp Type Type        -- ^ "Maybe a"
           | Bottom                -- ^ Most polymorphic type, i.e. "forall a. a"


-- | A quantifier. The type gives the bound which is 'Bottom' for normal quantifiers.
data Quant   = Quant Id Type


-- | Type variables represent substitutable parts of a type, only |Free| variables can be unified
data TypeVar = TypeVar Id Flavour

-- | A type variable comes in three flavours: bound, as a skolem constant, and unifiable variables
data Flavour = Quantified 
             | Skolem 
             | Uni (IORef Bound)  -- the updateable reference implements substitution

-- | The bound of unified variable.
data Bound   = Equal      Type            -- ^ it is equal to a certain type
             | Instance   Scheme Rank     -- ^ it can be an instance of some type

-- | The type variable rank is used for efficient generalization
-- | The rank corresponds with the depth of the earliest lamda binding that refers to
-- | this type variable. The depth of the outermost lambda binding in the environment is 0.
-- | We use an infinite rank (rankInf) for type variables that do not occur in the environment.
-- See: George Kuan and David McQueen, "Efficient ML type inference with ranked type variables"
type Rank = Int

-- | The 'infinite' rank is used for variables that do not occur in the environment.
-- | When unifying schemes, variables with a rank bigger than rankInf can be created, and the rank
-- | is then used to count the depth of the quantifiers.
rankInf :: Rank
rankInf = maxBound `div` 2

-- Accessors
tvFlavour (TypeVar _ fl) = fl
tvId      (TypeVar id _) = id
tvIds typeVars  = map tvId typeVars

tvFromId id = TypeVar id (error "Types.tvFromId")

quantId (Quant id _) = id
quantBound (Quant _ bound) = bound
quantIds qs = map quantId qs

isQuantified Quantified = True
isQuantified _     = False

isUni (Uni _) = True
isUni _         = False

isSkolem Skolem = True
isSkolem _      = False

boundNone 
  = Instance Bottom rankInf

splitFun :: Type -> Maybe (Type,Type)
splitFun tp
  = case tp of
      TApp (TApp (TCon "->") arg) res  
        -> Just (arg,res)
      _ -> Nothing




--------------------------------------------------------------------------
-- Helper functions 
--------------------------------------------------------------------------

normalize :: Type -> Type
normalize tp
  = tp

mkForall :: [Id] -> Type -> Type
mkForall ids tp
  = mkForallQ (mkQuants ids) tp

mkForallQ [] tp
  = tp
mkForallQ qs1 tp
  = case (splitQuants tp) of
      (qs2,tp2) | not (null qs2) -> mkForallQ (qs1 ++ qs2) tp2
      ([],TVar (TypeVar id Quantified))
          -> case [qtp | Quant qid qtp <- reverse qs1, qid == id] of
               (qtp:_)  -> qtp
               _        -> tp
      _   -> foldr Forall tp qs1

mkQuants ids
  = [Quant id Bottom | id <- ids]

splitQuants :: Type -> ([Quant],Type)
splitQuants tp
  = split [] tp
  where
    split qs tp
      = case tp of
          Forall q t  -> split (q:qs) t
          _           -> (reverse qs,tp)


mkAnnot :: [Id] -> Type -> Annot
mkAnnot ids tp
  = Annot (mkQuants ids) tp

mkFun :: Type -> Type -> Type
mkFun t1 t2
  = TApp (TApp (TCon "->") t1) t2

mkTuple :: Type -> Type -> Type
mkTuple t1 t2
  = TApp (TApp (TCon "(,)") t1) t2

mkList :: Type -> Type
mkList tp
  = TApp (TCon "[]") tp

-- | the "any type" annotation: (some a. a) 
annotAny :: Annot
annotAny
  = let id = 0 in Annot [Quant id Bottom] (TVar (TypeVar id Quantified))


--------------------------------------------------------------------------
-- Order preservering set operations on lists
--------------------------------------------------------------------------
union :: Eq a => [a] -> [a] -> [a]
union xs ys
  = xs ++ (ys `diff` xs)

diff :: Eq a => [a] -> [a] -> [a]
diff xs ys
  = [x | x <- xs, not (x `elem` ys)]

intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys
  = [x | x <- xs, x `elem` ys]

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint xs ys
  = all (\x -> not (x `elem` ys)) xs

subseteq :: Eq a => [a] -> [a] -> Bool
subseteq xs ys
  = all (`elem` ys) xs

remove :: Eq a => a -> [a] -> [a]
remove y xs
  = [x | x <- xs, x /= y]

--------------------------------------------------------------------------
-- Assertions
--------------------------------------------------------------------------
assert :: String -> Bool -> a -> a
assert msg test x
  = if test then x else error ("assertion failed: " ++ msg)


assertM :: Monad m => String -> Bool -> m ()
assertM msg test
  = assert msg test (return ())

--------------------------------------------------------------------------
-- Equality for type variables is based solely on the identifier
--------------------------------------------------------------------------
instance Eq TypeVar where
  (TypeVar id1 fl1) == (TypeVar id2 fl2)  = (id1 == id2)

instance Ord TypeVar where
  compare (TypeVar id1 fl1) (TypeVar id2 fl2)  = compare id1 id2



--------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------

instance Pretty Term where
  pretty term = ppTerm PrecTop term

instance Pretty Type where
   pretty tp  = ppTypeEx [] PrecTop tp

instance Pretty Annot where
   pretty ann = ppAnnot ann

instance Pretty TypeVar where
   pretty (TypeVar id Quantified)  = text "@" PPrint.<> pretty id
   pretty (TypeVar id Skolem)    = text "_" PPrint.<> pretty id
   pretty (TypeVar id (Uni _)) = text "$" PPrint.<> pretty id

instance Show Term where
  show t = show (pretty t)

instance Show Type where
  show t = show (pretty t)

instance Show TypeVar where
  show t = show (pretty t)

instance Show Annot where
  show ann = show (pretty ann)


--------------------------------------------------------------------------
-- Precedence
--------------------------------------------------------------------------
data Prec = PrecTop | PrecAnn | PrecFun | PrecOp | PrecApp | PrecAtom
          deriving (Eq,Ord,Enum)

precType :: Type -> Prec
precType tp
  = case tp of
      Forall _ _                       -> PrecTop
      TApp (TApp (TCon "->") arg) res  -> PrecFun
      TApp (TCon "[]") arg             -> PrecAtom
      TApp _ _                         -> PrecApp
      _                                -> PrecAtom

precTerm :: Term -> Prec
precTerm term
  = case term of
      App (Var "[]") _             -> PrecAtom                      
      App (App (Var "(,)") e1) e2  -> PrecAtom
      App (App (Var "$") e1) e2    -> PrecOp
      App (App (Var ":") e1) e2    -> case elements [e1] e2 of 
                                        Nothing -> PrecOp
                                        Just _  -> PrecAtom

      Let _ _ _     -> PrecTop
      Ann _ _       -> PrecAnn
      Lam _ _       -> PrecFun
      ALam _ _ _    -> PrecFun
      App _ _       -> PrecApp
      Rigid  _      -> PrecFun
      _             -> PrecAtom


--------------------------------------------------------------------------
-- pretty term
--------------------------------------------------------------------------
ppTerm :: Prec -> Term -> Doc
ppTerm prec term
  = (if (prec > precTerm term) then parens else id)
    (ppTermEx prec term)

ppTermEx :: Prec -> Term -> Doc
ppTermEx prec term
  = case term of
      App (App (App (Var "if") t1) t2) t3   -> hang 2 $ text "if" <+> ppTerm PrecApp t1 </> text "then" <+> ppTerm PrecApp t2 </> text "else" <+> ppTerm PrecApp t3
      App (App (Var "(,)") e1) e2           -> parens (ppTerm PrecTop e1 PPrint.<> comma PPrint.<> ppTerm PrecTop e2)
      App (App (Var "$") e1) e2             -> ppTerm PrecApp e1 <+> text "$" <+> ppTerm PrecTop e2
      App (App (Var ":") e1) e2             -> case elements [e1] e2 of 
                                                 Nothing -> ppTerm PrecApp e1 PPrint.<> text ":" PPrint.<> ppTerm PrecOp e2
                                                 Just es -> list (map (ppTerm PrecTop) es)
    
      App e1 e2      -> ppTerm PrecApp e1 <+> ppTerm PrecAtom e2
      Lam v e        -> char '\\' PPrint.<> ppLam "->" term 
      ALam v ann e   -> char '\\' PPrint.<> ppLam "->" term
      Let v rhs e    -> let (bs,body) = binders [(v,rhs)] e
                        in align $ text "let" <+> align (vcat (punctuate semi [text v <+> ppLam "=" rhs | (v,rhs) <- bs])) 
                                   PPrint.<> (if (length bs > 1) then line else softline)
                                   PPrint.<> text "in" <+> ppTerm PrecTop body
      Ann e ann      -> ppTerm PrecAtom e <+> text "::" <+> pretty ann
      Rigid e        -> text "rigid" <+> ppTerm PrecTop e
      Var n          -> text n
      Lit i          -> pretty i
  where
    binders acc (Let v rhs e)   = binders ((v,rhs):acc) e
    binders acc e               = (reverse acc, e)

    ppLam arrow (Lam v e)         = text v <+> ppLam arrow e
    ppLam arrow (ALam v ann e)    = parens (text v PPrint.<> text "::" PPrint.<> pretty ann) <+> ppLam arrow e
    ppLam arrow e                 = text arrow <+> ppTerm PrecFun e

elements acc (Var "[]")                 = Just (reverse acc)
elements acc (App (App (Var ":") e) es) = elements (e:acc) es
elements acc _                          = Nothing

--------------------------------------------------------------------------
-- Pretty type
--------------------------------------------------------------------------
ppAnnot (Annot qs tp)
  = let nice = niceExtend [] (quantIds qs)
    in (if null qs then empty else (text "some" <+> hsep (map (ppQuant nice) qs) PPrint.<> text ". ")) PPrint.<>
       ppType nice PrecTop tp
 


ppType :: [(Id,String)] -> Prec -> Type -> Doc
ppType nice prec tp
  = (if (prec > precType tp) then parens else id) (ppTypeEx nice prec tp)

ppTypeEx nice prec tp
  = case tp of
      Forall _ _
        -> let (qs,rho) = splitQuants tp
               nice'    = niceExtend nice (map quantId qs)
           in text "forall" <+> hsep (map (ppQuant nice') qs) PPrint.<> dot <+>
              ppType nice' PrecTop rho
      
      TApp (TApp (TCon "->") arg) res  -> ppType nice PrecApp arg <+> text "->" <+> ppType nice PrecFun res
      TApp (TCon "[]") arg             -> brackets (ppType nice PrecTop arg)
      TApp (TApp (TCon "(,)") t1) t2   -> parens (ppType nice PrecTop t1 PPrint.<> text "," PPrint.<> ppType nice PrecTop t2)
      TApp tp arg                      -> ppType nice PrecApp tp <+> ppType nice PrecAtom arg
      
      TVar (TypeVar id Quantified)     -> ppNice nice id
      TVar (TypeVar id (Uni ref))      -> text "$" PPrint.<> ppNice nice id PPrint.<> ppBound nice ref
      TVar (TypeVar id Skolem)         -> text "_" PPrint.<> pretty id

      TCon name                        -> text name
      Bottom                           -> text "_|_"

ppRank :: Rank -> Doc
ppRank r
  = if r == maxBound then text "inf" else pretty r

ppQuant nice (Quant id tp)
  = case tp of
      Bottom -> ppNice nice id
      _      -> parens (ppNice nice id <+> text ">=" <+> ppType nice PrecTop tp)

ppBound nice ref
  = case unsafePerformIO (readIORef ref) of
      Equal tp             -> text "=" PPrint.<> ppType nice PrecAtom tp
      Instance Bottom rank -> text "_" PPrint.<> ppRank rank
      Instance tp rank     -> text "_" PPrint.<> ppRank rank PPrint.<> text ">=" PPrint.<> parens (ppType nice PrecTop tp)

--------------------------------------------------------------------------
-- Pretty print bound identifiers nicely
--------------------------------------------------------------------------
ppNice nice id
  = case lookup id nice of
      Nothing     -> text (show id) -- this can happen due to updateable references :-( 
      Just name   -> text name

niceExtend :: [(Id,Name)] -> [Id] -> [(Id,Name)]
niceExtend nice ids
  = zip ids (drop (length nice) niceNames) ++ nice
 

niceNames :: [String]
niceNames 
  = [[c] | c <- ['a'..'z']] ++ [[c]++show i | i <- [1::Int ..], c <- ['a'..'z']]

