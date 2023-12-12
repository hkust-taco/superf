--------------------------------------------------------------------------
-- The type environment Gamma
--------------------------------------------------------------------------
module Gamma( Gamma
            , gamma0  -- the initial gamma
            , gammaFind
            , gammaExtend
            , gammaCoDomain
            ) where

import qualified Data.Map as Map
import Types
import Parser( readType )


--------------------------------------------------------------------------
-- initial gamma 
--------------------------------------------------------------------------
gamma0 :: Gamma
gamma0
  = gammaCreate
      -- builtin functions
      [("(,)", "forall a b. a -> b -> (a,b)")
      ,("[]",  "forall a. [a]")
      ,(":",   "forall a. a -> [a] -> [a]")
      ,("$",   "forall a b. (a -> b) -> a -> b")
      ,("if", "forall a. Bool -> a -> a -> a")
      
      -- standard functions
      ,("id", "forall a. a -> a")
      ,("apply", "forall a b. (a -> b) -> a -> b")
      ,("const", "forall a b. a -> b -> a")
      ,("choose", "forall a. a -> a -> a")
      ,("revapp", "forall a b. a -> (a -> b) -> b")
      ,("undefined", "forall a. a")
      
      -- booleans
      ,("True", "Bool")
      ,("False", "Bool")

      -- integers
      ,("plus", "Int -> Int -> Int")
      ,("lt", "Int -> Int -> Bool")
      ,("gt", "Int -> Int -> Bool")
      ,("inc", "Int -> Int")
      
      -- polymorphic functions
      ,("ids", "[forall a. a -> a]")
      ,("auto", "(forall a. a -> a) -> (forall a. a -> a)")
      ,("xauto", "forall a. (forall b. b -> b) -> a -> a")

      ,("takeAuto", "((forall a. a -> a) -> (forall a. a -> a)) -> (forall a. a -> a)")
      
      -- lists
      ,("single", "forall a. a -> [a]")
      ,("head", "forall a. [a] -> a")
      ,("tail", "forall a. [a] -> [a]")
      ,("map", "forall a b. (a -> b) -> [a] -> [b]")
      ,("length", "forall a. [a] -> Int")
      ,("null", "forall a. [a]-> Bool")
      ,("append", "forall a. [a] -> [a] -> [a]")

      -- tuples
      ,("fst", "forall a b. (a,b) -> a")
      ,("snd", "forall a b. (a,b) -> b")

      -- ST monad
      ,("runST", "forall a. (forall s. ST s a) -> a")
      ,("newRef", "forall a s. a -> ST s (Ref s a)")
      ,("returnST", "forall a s. a -> ST s a")

      -- SuperF additions
      ,("unit", "Unit")
      ,("eq", "forall a. a -> a -> Bool")
      ,("sub", "Int -> Int -> Int")
      ,("fix", "forall a. (a -> a) -> a")
      ,("nil", "forall a. [a]")
      ,("cons", "forall a. a -> [a] -> [a]")
      ,("append", "forall a. [a] -> [a] -> [a]")
      ,("poly", "(forall a. a -> a) -> (Int,Bool)")
      ,("auto_", "forall a. (forall b. b -> b) -> a -> a")
      ,("app", "forall a b. (a -> b) -> a -> b")
      ,("argST", "forall s. ST s Int")
      ,("zero", "forall a. (a -> a) -> (a -> a)")
      ,("succ", "(forall a. (a -> a) -> (a -> a)) -> (forall a. (a -> a) -> (a -> a))")
      ,("f", "forall a. (a -> a) -> [a] -> a")
      ,("g", "forall a. [a] -> [a] -> a")
      ,("k", "forall a. a -> [a] -> a")
      ,("h", "Int -> (forall a. a -> a)")
      ,("l", "[forall a. Int -> a -> a]")
      ,("r", "(forall a. a -> (forall b. b -> b)) -> Int")
      ,("const", "forall a b. a -> b -> a")
      ]
    

--------------------------------------------------------------------------
-- Gamma is a mapping from (term) variables to types 
--------------------------------------------------------------------------

-- | Gamma maps term variables to types
data Gamma  = Gamma (Map.Map Name Type)


gammaCreate :: [(String,String)] -> Gamma
gammaCreate bindings
  = gammaFromList $ [(name,readType tp) | (name,tp) <- bindings]

gammaEmpty 
  = Gamma Map.empty 

gammaFromList xs
  = Gamma (Map.fromList xs)

gammaFind (Gamma g) name
  = case Map.lookup name g of
      Nothing -> error ("unbound variable: " ++ name)
      Just tp -> tp

gammaExtend (Gamma g) name tp
  = Gamma (Map.insert name tp g)

instance Show Gamma where
  show (Gamma g)  = show g


gammaCoDomain :: Gamma -> [Type]
gammaCoDomain (Gamma g)
  = Map.elems g
