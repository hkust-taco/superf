--------------------------------------------------------------------------
-- Type inference
--
-- Basic type inference algorithm for HML as described in the paper.
--------------------------------------------------------------------------
module InferBasic( inferType, features ) where

import PPrint
import Types
import Operations   
import Unify        
import Gamma

features :: [Feature]
features = []
--------------------------------------------------------------------------
-- Infer the type of a term
--------------------------------------------------------------------------
inferType :: Term -> IO Type
inferType term
  = runInfer $
    do uniqueReset
       infer gamma0 term


infer :: Gamma -> Term -> Infer Type
infer gamma (Var name)
  = return (gammaFind gamma name)

infer gamma (Lit i)
  = return (TCon "Int")

infer gamma (Let name expr body)
  = do tp  <- infer gamma expr
       infer (gammaExtend gamma name tp) body

infer gamma (Lam name expr)
  = do -- we can treat this as an annotated lambda expression: (\x -> e) => (\(x::some a. a) -> e)        
       infer gamma (ALam name annotAny expr)       

infer gamma (ALam name ann expr)
  = do (tp1,tp2) <- withRankIncrease $                       -- we are going inside a lambda
                    do (some,tp1) <- instantiateAnnot ann    -- instantiate the "some" quantifiers
                       tp2        <- infer (gammaExtend gamma name tp1) expr       
                       -- check that we don't infer polytypes for arguments        
                       taus  <- mapM isFlexTau some
                       check (and taus) ("Using unannoted parameter(s) polymorphically with type(s): " ++ show some)  
                       return (tp1,tp2)       
       -- generalize the result
       generalize gamma $
        do res <- freshInstanceVar tp2
           return (mkFun tp1 res)
      
infer gamma (App e1 e2)
  = do tp1       <- infer gamma e1
       tp2       <- infer gamma e2
       generalize gamma $
        do rho1 <- instantiate tp1
           res  <- freshInstanceVar Bottom 
           arg  <- freshInstanceVar tp2 
           -- message $ "function unify: " ++ show rho1 ++ " and  " ++ show tp2
           unify rho1 (mkFun arg res)
           return res

infer gamma (Ann expr ann)
  = do -- we can treat annotations just as applications to an annotated identity function: (e::s) => ((\(x::s).x) e)
       infer gamma (App (ALam "x" ann (Var "x")) expr)

infer gamma (Rigid expr)
  = infer gamma expr















