
This is a small but optimized reference implementation of the HML type system
described in the paper "Flexible types: Robust type inference for first-class polymorphism",
available at "http://research.microsoft.com/users/daan/pubs.html"

This implementation uses IO references to implement substitution instead of using
explicit substitutions. Also, it uses ranked unification type variables to implement
efficient generalization.

Just load the prototype in GHCi as:

  > ghci Main.hs

and from the GHCi prompt, run the tests:

  *Main> testAll
  ...

or check some expressions yourself:

  *Main> check "\\x -> x"
  expr: \x -> x
  type: forall a. a -> a

  *Main> check "auto"
  expr: auto
  type: forall a. (forall b. b -> b) -> a -> a

  *Main> check "apply auto id"
  expr: apply auto id
  type: forall a. a -> a

  *Main> check "map auto ids"
  expr: map auto ids
  type: forall a. [a -> a]

or get some more help:

  *Main> help
  ...

See the test-cases in "Main.hs" for many more examples, 
and "Gamma.hs" for the standard available functions.

Have fun,
-- Daan Leijen.


Note: At the moment, it does not yet use canonical forms and some programs are incorrectly 
rejected depending on the order of quantifiers.



Modules:
--------------------------------------------------------------------------------------------
PPrint.hs           Pretty printer library
Types.hs            Basic definitions of terms and types
Parser.hs           Basic parser for terms and types
Subst.hs            Explicit substitutions
Gamma.hs            Type environment: also contains standard functions like "const", or "id"
Operations.hs       Basic type operations: instantiate, skolemize etc.
Unify.hs            Unification, subsumption, and unifyScheme algorithms

InferBasic.hs       Basic HML type inference  
Main.hs             Main module

