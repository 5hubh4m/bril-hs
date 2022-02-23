# bril-hs

This is a library to process Bril programs in Haskell. It includes
a set of types that contain a Bril AST and utilities to convert the
JSON representation of Bril programs to and from the AST along with
testing utilities.

## Bril

This is the top level package that contains the modules.

### Bril.Lang

This package contains two modules `Bril.Lang.AST` and `Bril.Lang.Parse`
which contain code for represending Bril programs in a type-safe way and
convert that representation to/from JSON.

### Bril.Structure

This package contains the module `Bril.Structure.CFG` which contains
utilities for converting a Bril program into basic blocks and CFG
and performing operations on them like finding dominators of each basic
block etc.

## Building and Using

The package is built using `stack`.

Currently the `Main.hs` file implements dominator utilities. It takes in
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

The directory `test` contains `QuickCheck` specifications for the utility
functions. Run them using `stack`.

```
stack test
```
