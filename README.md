# bril-hs

This is a library to process [Bril](https://capra.cs.cornell.edu/bril/) programs
in Haskell. It includes a set of types that contain a Bril AST and utilities
to convert the JSON representation of Bril programs to and from the AST along with
testing utilities.

## Bril

This is the top level package that contains the modules.

### Bril.Lang

This package contains two modules [`Bril.Lang.AST`](src/Bril/Lang/AST.hs) and
[`Bril.Lang.Parse`](src/Bril/Lang/Parse.hs) which contain code for
represending Bril programs in a type-safe way and convert that
representation to/from JSON.

### Bril.Structure

This package contains the module [`Bril.Structure.CFG`](src/Bril/Structure/CFG.hs)
which contains utilities for converting a Bril program into basic blocks and CFG
and performing operations on them like finding dominators of each basic
block etc.

## Building and Using

The package is built using [`stack`](https://docs.haskellstack.org/en/stable/README/)
which can be installed using `brew install haskell-stack` on macOS.

Currently the [`Main.hs`](app/Main.hs) file implements dominator utilities. It takes in
a JSON Bril program and prints the output of a dominator utility. There is
support for the following utilities:

* `doms`: Outputs the dominators of each function.

  `dominator :: CFG -> HashMap Ident (HashSet Ident)`

* `tree`: Outputs the dominator tree of each function.

  `dominationTree :: CFG -> Tree Ident`

* `front`: Outputs the domination frontier of each function.

  `dominationTree :: CFG -> HashMap Ident (HashSet Ident)`

To use the utilities, build and run with the appropriate argument

```
stack build
bril2json < path/to/bril/program | stack run [doms | tree | front]
```

## Testing

The directory [`test`](test/) contains [QuickCheck](https://hackage.haskell.org/package/QuickCheck)
specifications for the [utility functions](test/Bril/Structure/Spec.hs). Run them using `stack`.

```
stack test

...
bril-hs> test (suite: bril-hs-test)

=== prop_dominatorsDefn from test/Bril/Structure/Spec.hs:75 ===
+++ OK, passed 10000 tests.

=== prop_dominationTreeDefn from test/Bril/Structure/Spec.hs:91 ===
+++ OK, passed 10000 tests.

=== prop_dominationFrontierDefn from test/Bril/Structure/Spec.hs:107 ===
+++ OK, passed 10000 tests.

bril-hs> Test suite bril-hs-test passed
Completed 2 action(s).
```
