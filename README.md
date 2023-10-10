# POPL 2024 Artifact for: When Subtyping Constraints Liberate: A Novel Approach to First-Class-Polymorphic Type Inference


This document follows the structure prescribed in https://popl24.sigplan.org/track/POPL-2024-artifact-evaluation.


# List of claims

Most claims in the paper have to do with reproducing particular examples in the test suite. For each such claim, you can check that the inferred types conform with what is shown in the paper, and you can play with the examples by modifying them directly in the test suite or just trying them out individually in the online web demo available at https://hkust-taco.github.io/superf/, which also contains a syntax reference.


### Claim 1: Individual Examples Shown in the Paper

All examples shown inline throughout the paper and appendices are reproduced in the test suite.
Some are in the `shared/src/test/diff/fcp/Demo.mls` file, and others are in `shared/src/test/diff/fcp/Paper.mls`.


### Claim 2: Summary Table of Examples from Previous Literature

The tables in Appendices B.4 and B.5 are reproduced in `shared/src/test/diff/fcp/PaperTable.mls`.

Other tests from previous systems, which were not necessarily part of the corresponding papers themselves but which we gleaned from various sources, live in the `shared/src/test/diff/fcp-lit` directory.


### Claim 3: MLF test suite

We have obtained the original test suite of MLF, graciously provided to us by the MLF authors.
These tests have been faithfully ported to SuperF and all live in the `shared/src/test/diff/mlf-examples` folder.


### Claim 4: Validation on existing MLscript test suite

The tests in folders `shared/src/test/diff/fcp/`, `shared/src/test/diff/fcp-lit/`, and `shared/src/test/diff/mlf-examples/` are specifically meant to test first-class polymorphism (FCP) and usually use `:NoRecursiveTypes` (see description of the command below).
Most tests in other folders predate the introduction of FCP in MLscript. Yet FCP type inference is still performed in these tests,
showing that the addition of FCP was smooth and did not break most existing tests (the main exception being tests involving variations of the Y combinator through unannotated self applications).



### Claim 5: Validation on Existing OCaml Code

We have manually transcribed the OCaml List module in file `shared/src/test/diff/fcp/OCamlList.mls`,
showing that we can type check that file without the help of any intermediate type annotations.

The code is in three parts:

 1. Specification of general OCaml definition signatures.
 2. Specification from OCaml's [`List.mli`](https://github.com/ocaml/ocaml/blob/5312b4d7b913cde2a69fc0eb5e97e353865b82df/stdlib/list.mli) signature file.
 3. Impelemntations from OCaml's [`List.ml`](https://github.com/ocaml/ocaml/blob/5312b4d7b913cde2a69fc0eb5e97e353865b82df/stdlib/list.ml).





# Download and Installation

## Using the Docker Image

We have built Docker images containing all necessary dependencies
compiled for both `amd64` and `arm64` platforms and pushed them to
[DockerHub](https://hub.docker.com/r/mlscript/superf-docker/tags).
One can pull the `amd64` image and launch a container with the following command:

```
docker pull mlscript/superf-docker:v0-amd64
docker run -it --rm mlscript/superf-docker:v0-amd64
```

If one wants to use the `arm64` image, the commands are:

```
docker pull mlscript/superf-docker:v0-arm64
docker run -it --rm mlscript/superf-docker:v0-arm64
```

The user will be attached to the shell of the container after the image gets
pulled and the container is launched.
Please `cd` to `mlscript/` and launch the SBT shell by typing `sbt`.

## Setting up from Scratch

### Prerequisites

You need [JDK supported by Scala][supported-jdk-versions], [sbt][sbt], [Node.js][node.js], and TypeScript to compile the project and run the tests.

We recommend you to install JDK and sbt via [coursier][coursier]. The versions of Node.js that passed our tests are from v16.14 to v16.17, v17 and v18. Run `npm install` to install TypeScript. **Note that ScalaJS cannot find the global installed TypeScript.** We explicitly support TypeScript v4.7.4.

[supported-jdk-versions]: https://docs.scala-lang.org/overviews/jdk-compatibility/overview.html
[sbt]: https://www.scala-sbt.org/
[node.js]: https://nodejs.org/
[coursier]: https://get-coursier.io/

---

If one would like to run our implementation on their host system from scratch,
please follow the following steps:

1. Install NodeJS (for example `v16.17`) from https://nodejs.org/en/download/.
   Ensure your PATH is updated and the `node` command is available in the terminal.

2. Follow the Coursier installation instructions at https://get-coursier.io/docs/cli-installation.

3. Change your working directory to the root of this repository and
   launch the SBT shell by typing `sbt` in the terminal.

## Project Structure

### Sub-Projects

- The ts2mls sub-project allows you to use TypeScript libraries in MLscript. It can generate libraries' declaration information in MLscript by parsing TypeScript AST, which can be used in MLscript type checking.

### Directories

- The `shared/src/main/scala/mlscript` directory contains the sources of the MLscript compiler.

- The `shared/src/test/scala/mlscript` directory contains the sources of the testing infrastructure.

- The `shared/src/test/diff` directory contains the actual tests.

- The `ts2mls/js/src/main/scala/ts2mls` directory contains the sources of the ts2mls module.

- The `ts2mls/js/src/test/scala/ts2mls` directory contains the sources of the ts2mls declaration generation test code.

- The `ts2mls/jvm/src/test/scala/ts2mls` directory contains the sources of the ts2mls diff test code.

- The `ts2mls/js/src/test/typescript` directory contains the TypeScript test code.

- The `ts2mls/js/src/test/diff` directory contains the declarations generated by ts2mls.

# Evaluation Instructions


## Running the tests

### Commands and validating our claims

Running the main MLscript tests only requires the Scala Build Tool installed.
In the terminal, run `sbt mlscriptJVM/test`.

Running the ts2mls MLscript tests requires NodeJS, and TypeScript in addition.
In the terminal, run `sbt ts2mlsTest/test`.

You can also run all tests simultaneously.
In the terminal, run `sbt test`.

Test output is inserted **in place** in the test file after each corresponding block, as comments beginning with `//│`.
This makes it very easy and convenient to see the test results for each code block.
For this reason, we recommend using an editor that automatically reloads open files on changes. VSCode and Sublime Text work well for this.

To validate each of our claims, please refer to the corresponding MLscript test sources
and check the inferred types with those shown in the paper.
Note that the syntax of the inferred types of our implementation
is slightly different than that in the paper. Please refer to the following
"Notes on inferred type syntax".

The testing infrastructure is set up so that if there are any unstaged changes (as determined by `git`) in any test file
(those in `shared/src/test/diff`), only the corresponding files will be tested.
So you can make select modifications to some test files and run the test command again,
and it will only run your modified tests.

You can also run the tests continuously using the SBT command `~mlscriptJVM/testOnly mlscript.DiffTests`,
which will watch for any changes in the project and re-run the tests automatically.


### Notes on inferred type syntax

- `anything` is the top type
- `nothing` is the bottom type
- `?`, as in `List[?]`, is a shorthand for `List['a]` for some fresh `'a` quantified at the top-level
- The notation `??x`, which sometimes appear in examples that leak skolems, is really just a way of keeping track of extrusions in order to better pretty-print infer type and to better report type errors, using type variable names that are more informative than ⊤ and ⊥. The syntax can be understood as attaching to those ⊤ and ⊥ introduced during extrusion the names of the corresponding extruded type variables as metadata.

### Providing type signatures

When a type signature was declared before and an implementation is provided, our compiler infers the implementation's type first, and then checks that type against the signature, which leads to an output of the following form:
```fsharp
def idInt : int -> int
//│ idInt: int -> int

def idInt x = x
//│ 'a -> 'a
//│   <:  idInt:
//│ int -> int
```
Above, one can see that the _inferred_ type is `'a -> 'a` (i.e., `forall 'a. a -> 'a`), and this is successfully checked against the provided type signature `int -> int`.

## Running tests individually

Individual tests can be run with `-z`.
For example, `~mlscriptJVM/testOnly mlscript.DiffTests -- -z parser` will watch for file changes and continuously run all parser tests (those that have "parser" in their name).


## Running the web demo locally

To run the demo on your computer, compile the project with `sbt fastOptJS`, then open the `local_testing.html` file in your browser.

You can make changes to the type inference code
in `shared/src/main/scala/mlscript`,
have it compile to JavaScript on file change with command
`sbt ~fastOptJS`,
and immediately see the results in your browser by refreshing the page with `F5`.




# Additional Artifact Description

We now describe at a high level how to understand the MLscript codebase.


## Introduction

In this artifact, we provide a fully practical implementation of SuperF.
The provided implementation is complex
because it is intertwined with MLscript, a nascent functional programming language intended for real-world usage.
While this has the advantage of demonstrating that our approach scales to realistic use cases and is practical for general-purpose programming, it means one needs to squint quite a bit to relate the provided implementation with the approach described in the paper.

We have reproduced here the main differences between the paper's formalism and our implementation,
as stated in Appendix B:
- MLscriptF supports type-checking recursive functions, which is not shown in Section 4 for
brevity, as it is very straightforward: we simply assign the recursive definition a fresh type
variable, bind that type variable to the definition’s name while typing the definition’s body,
and then constrain the variable to be a supertype of the inferred body type.
- MLscriptF does not generalize curried lambda abstractions: doing so is redundant since
distributivity can always be used to push polymorphic types back inside arrow types when
needed. A slightly more subtle point is that we do not want to incur too much polymorphism
in the presence of recursive terms, as that could lead to spurious SRLC errors.22 Refraining
from generalizing curried functions is a simple way to avoid this problem.
- MLscriptF uses explicit polymorphism levels to track extrusion and type avoidance, as described in Section 2.4. This can be seen as an optimization (to minimize the number of type
traversals) that does not affect the functional properties of the system.

In addition to that, by default MLscript supports recursive types, but these are disabled in the tests relevant to this paper (because the system presented in our paper does not support them), using the `:NoRecursiveTypes` top-level command in the corresponding test files.

Also take note of the `:DistributeForalls` (on by default) and `:DontDistributeForalls` commands which enable and disable distributivity of polymorphic types over function types. 

The most important algorithms to look at which (loosely) mirror what happens in the paper are:
- the `typeTerm` function of `Typer.scala`, which shows how to type check terms;
- the `constrain` function of `ConstraintSolver.scala`, which shows how to resolve subtyping constraints (while also keeping track of a lot of meta information for better error reporting);
- `freshenAbove` in `ConstraintSolver.scala`, which instantiates a polymorphic type by duplicating all its quantified type variables (those with a level higher than the quantification level).
- `extrudeAndCheck` in `ConstraintSolver.scala`, which performs extrusion of types with mismatched polymorphism level, creating approximants and transforming leaky skolems into the equivalent of Top and Bottom (using the `Extruded` data type).

A major concept that you will encounter repeatedly if you look into the MLscript's type checker implementation but which are absent from the paper is that of *normal forms* (defined in `NormalForms.scala`).
These are used for two things:
 - For type simplification before showing inferred types to users. Our submission does not give specific details on how we simplify types because it is by and large similar to how it was done in previous work
 - For constraint-solving involving non-polar lattice types (ie unions in negative positions and intersections in positive positions) – subfunctions `goToWork`, `constrainDNF`, and `annoying` in `ConstraintSolver.scala` are the ones that are invoked in these cases. Such constraints never come up on their own if one restricts themselves to the polar type annotation syntax proposed in the paper. Indeed, only unions in positive positions and intersections in negative positions naturally arise from type inference. Therefore, normal-form constraint solving can be safely ignored while reviewing this artifact with SuperF in mind.


In the interest of transparency, and to maximize the usefulness and reusability of our artifact,
we have tagged, in comments, some of the test cases with notable characteristics. The following can easily be grepped in the test folder:
 * `[FCP-LIM]` is used to indicate a limitation of our FCP implementation relative to MLF. The only current uses of this tag are for (1) programs that requrie generalization at the level of arguments (which is disabled by default but can be enabled with `:GeneralizeArguments`) – see file `shared/src/test/diff/mlf-examples/ex_predicative.mls` – and (2) programs where unions are used in annotations, which is not really supported by SuperF, as explained in the paper – see file `shared/src/test/diff/mlf-examples/ex_shallow.mls`.
 * `[FCP-IMPROV]` shows some notable places where we significantly improve on MLF – but these are by no means meant to be exhaustive nor representative (indeed, many improvements are not explicitly tagged).
 * `[FCP:patho]` is used to tag two knwon pathological cases exposed in the test suite – in both cases, these correspond to programs that are fully unannotated (and thus would simply not work in existing approach) and involve complicated higher-order encodings not really representative of real-world programming patterns.




## Overview of the MLscript codebase

This codebase has all the basic components
of a static-typed programming language compiler: lexer, parser, typer, and code generator.
We now give a high-level introduction to each compiler component and how it corresponds to
what is described in the paper.
Note that the source files we describe here are all rooted in `/shared/src/main/scala/mlscript`.

### Lexing

The lexer accepts source strings and returns tokens to be parsed.
The corresponding files are:

- `NewLexer.scala` contains the lexer class.
- `Token.scala` contains the token data types.

### Parsing

The parser accepts tokens generated by the lexer and
returns an abstract syntax tree of the input program in the surface syntax.
The corresponding files are:

- `NewParser.scala` contains the parser class.
- `syntax.scala` contains the **surface** syntax data types of the language.

### Typing

The typer accepts an abstract syntax tree of a program
and performs type checking.
MLscript's typer supports principal type inference with subtyping.
For more information about the type system,
please refer to [MLstruct](https://dl.acm.org/doi/abs/10.1145/3563304).

The corresponding files are:
- `Typer.scala` contains the typer class.
- `TypeSimplifier.scala` contains type simplification algorithms to simplify
inferred types.
- `ucs/Desugarer.scala` contains class `ucs.Desugarer` which implements desugaring
methods.
- `TypeDefs.scala` and `NuTypeDefs.scala` contain class `TypeDef` and methods for
declarations like classes, interfaces, and type aliases.
- `ConstraitSolver.scala` contains class `ConstraintSolver` which solves subtyping
constraints.
- `NormalForms.scala` contains class `NormalForms` which provides the infrastructure
to solve tricky subtyping constraints with disjunct normal forms (DNF) on the left
and conjunct normal forms (CNF) on the right.
- `TyperDatatypes.scala` contains class `TyperDatatypes` which includes
data types to support **internal** representation of types with mutable states to support
type inference with subtyping.
- `TyperHelpers.scala` contains class `TyperHelpers` that provides helper methods
for the typer.

Note that the inheritance relationships between these typer classes do *not* have any actual semantics
- we are following Scala's *Cake Pattern*. Typer classes will be finally composed
into the `Typer` class by inheritance.

### Code Generation

The code generator translates MLscript AST into JavaScript AST and generates the corresponding JavaScript code.

The corresponding files are:

- `codegen/Codegen.scala` contains definitions of JavaScript AST nodes
  and methods for JavaScript code generation.
- `codegen/Scope.scala` contains class `Scope` which manages symbols
  and provides hygienic runtime name generation.
- `codegen/Symbol.scala` contains classes `NewClassSymbol`, `MixinSymbol`,
  and `ModuleSymbol` which include information on `class`, `mixin` and `module` definitions.
- `JSBackend.scala` contains class `JSBackend` that translates an MLscript AST
  into a JavaScript AST. Classes `JSWebBackend` and `JSTestBackend` inherit class `JSBackend`
  and generate adapted code for the web demo and the test suite.

### Web Demo and Testing


Testing of MLscript works as follows:
 - the MLscript compiler reads the given test file one code block at a time (code blocks are separated by empty lines);
 - after reading the code block, it outputs the inferred types as well as any type errors encountered;
 - after that, it executes the code block in NodeJS (by shelling out to a `node` process) and outputs the results.

We have a web demo for users to test our implementation in any modern browser.
It has a textbox for MLscript source code input and it produces typing and running
results live. The implementation can be tried online at https://hkust-taco.github.io/superf/
and locally in `/js/src/main/scala/Main.scala`.

We have a "`diff`-based" test suite for our implementation.
It detects changes to MLscript test sources (using git),
generates typing and running results, and inserts those results
into test sources. The diff-based testing implementation is in
`/shared/src/test/scala/mlscript/DiffTests.scala`.
MLscript test sources are in `/shared/src/test/diff`.

## Detailed Introduction

We now introduce the implementation of each compiler component
in more detail.

### Lexing

Class `NewLexer` in `NewLexer.scala` is the lexer class. It takes an `origin` object,
which contains the original source string together with the source file name,
the number of the first line, and some helper functions. Lazy value `tokens` generates
a list of tokens with their location in the source code. Lazy value `bracketedTokens`
converts the lexed tokens into *structured tokens*,
which use `BRACKETS` constructs instead of `OPEN_BRACKET`/`CLOSE_BRACKET` and `INDENT`/`DEINDENT`.
Token and structured token data types can be found in `Tokens.scala`.

### Parsing

Class `NewParser` in `NewParser.scala` is the parser class. It takes a list
of structured tokens with their location information. Method `typingUnit`
calls method `block` to parse the token list into a list of `Statement` or
`IfBody` (defined in `syntax.scala`), filters out unexpected `then/else`
clauses introduced by `Ifbody`, and returns a `TypingUnit` (a list of `Statement`).

File `syntax.scala` contains *immutable* surface syntax data types of MLscript,
which are different from the internal representations in the typer for later type inference.
Here we introduce several surface syntax data types:

- Classes `Decl`, `TypeDef`, `MethodDef` are deprecated.
- Class `TypeDefKind` includes type definition kinds: classes and mixins, etc.
- Class `Term` includes MLscript term data types. Case class `Bind` is no longer used.
Case class `Splc` is for the rest of a parameter list, similar to the rest parameter in JavaScript.
Case classes `Forall` and `Inst` are for first-class polymorphism.
- Class `IfBody` includes if-then-else structure data types.
- Class `CaseBranches` includes case branch data types for MLscript pattern matching.
- Class `TypeLike` includes `Type`, `Signature`, and `NuDecl`.
- Class `Type` includes MLscript type data types. Case class `Rem` is for record member removal.
Case class `WithExtension` is for record type extension. For example, `A with {x : int}`
is equivalent to `A\x & {x : int}`.
- Class `TypeVar` represents the type variable. Its identifier can be an `Int`
generated internally by the compiler or `Str` specified by the user.
- Class `NuTypeDef` is a `NuDecl` for type definitions.
Note that it has optional `superAnnot`
and `thisAnnot` for precisely-typed open recursion.
- Class `NuFunDef` is a `NuDecl` for function and let-bindings.

### Typing

The MLscript typer (class `Typer`) works with a typing context (class `Ctx`) which
mainly maintains all global and local bindings of names to their types.
The typer accepts a typing unit from the parser, types the typing unit, and returns a typed typing unit.
The typed typing unit
is sent to the type simplifier and is finally expanded, i.e., converted
back to types in the surface syntax for presentation.
The typer has **internal** representations of types
(defined in `TyperDatatypes.scala`)
with mutable states for type inference with subtyping. 

We first introduce several typer data types defined in `TyperDatatypes.scala`:

- Class `TypeProvenance` stores the location where a type is introduced.
- Class `LazyTypeInfo` is for type definitions including classes, mixins, modules.
Its type is lazily computed to support *mutual recursive* type
definitions. It has a `complete` method to complete typing lazily typed definitions.
- Class `PolymorphicType` represents a type with universally quantified type variables.
By convention, in the type body, type variables of levels greater than
the polymorphic type's level are polymorphic.
- Class `SimpleType` is a general type form of all types.
It requires a method `level` for level-based polymorphism.
- Class `BaseType` includes base types such as function, array, tuple, and class tag types.
It can later be refined by `RecordType`.
- Class `RecordType` is a record type. It has a list of bindings from record fields to their types.
- Class `SpliceType` is not used for now.
- Class `ProxyType` is a derived type form to store more type provenance information.
- Class `TypeRef` is a reference to named types such as type definitions like classes.
It has a list of type arguments. A type reference with type arguments is expanded to
a class tag type with the class's type parameters refined by the corresponding type arguments as type members.
For example, `Foo[Int]` is expanded to `#Foo & {Foo.A: int..int}`.
- Class `TypeTag` has different kinds of type tags including class tags and abstract tags, etc.
- Class `FieldType` represents a term field type or a type member.
When it represents a term field type, `lb` represents if the type is mutable.
Otherwise, `lb` is the lower bound of the type member.
- Class `TypeVariable` represents a type variable, which has upper and lower bounds
for type inference with subtyping.

Method `typeTypingUnit` in class `NuTypeDefs` accepts the typing unit to type. It inspects each statement
in the typing unit. If the statement is a type definition, a `DelayedTypeInfo` (which is a subclass of `LazyTypeInfo`)
is produced and stored in the typing context (note the typing context only uses `tyDefs2` to store
type definitions). Otherwise, it desugars the statement and calls `typeTerms` to type
the desugared statements. For a single `Term`, it is passed to `typeTerm` to type.
Method `typeTerm` in class `Typer` types a term. If the term needs type information of a `LazyTypeInfo`,
the typing of that lazily typed definition will be completed. Subtyping constraints are generated during typing
and sent to `ConstraintSolver` to propagate constraints to type variables.
For more about type inference of subtyping, please refer to [MLstruct](https://dl.acm.org/doi/abs/10.1145/3563304).





