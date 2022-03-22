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

The module [`Bril.Structure.CFG`](src/Bril/Structure/CFG.hs)
contains utilities for converting a Bril program into basic blocks and CFG
and performing operations on them like finding dominators of each basic
block etc.

The module [`Bril.Structure.SSA`](src/Bril/Structure/SSA.hs)
exports a function `ssa` for converting a Bril program into SSA form
by adding `phi` instruction and renaming variables and the function `ssa'`
which removes all the `phi` instructions and replaces with variable copies.

The module [`Bril.Structure.Loop`](src/Bril/Structure/Loop.hs)
exports functions to extract all the natural loops in a Bril program.

## Building and Using

The package is built using [`stack`](https://docs.haskellstack.org/en/stable/README/)
which can be installed using `brew install haskell-stack` on macOS.

Currently the [`Main.hs`](app/Main.hs) file implements dominator utilities and SSA conversion.
It takes in a JSON Bril program and outputs the result.

```
$> stack build
$> bril2json < path/to/bril/program | stack run [doms | front | tree | ssa | ssa']
```

## Testing

The directory [`spec`](spec/) contains [QuickCheck](https://hackage.haskell.org/package/QuickCheck)
specifications for the [utility functions](spec/Bril/Structure/Spec.hs). Run them using `stack`.

```
$> stack test

...
bril-hs> test (suite: bril-hs-test)

=== prop_dominatorsDefn from spec/Bril/Structure/Spec.hs:75 ===
+++ OK, passed 10000 tests.

=== prop_dominationTreeDefn from spec/Bril/Structure/Spec.hs:91 ===
+++ OK, passed 10000 tests.

=== prop_dominationFrontierDefn from spec/Bril/Structure/Spec.hs:107 ===
+++ OK, passed 10000 tests.

bril-hs> Test suite bril-hs-test passed
Completed 2 action(s).
```

### Testing SSA Conversion

The directory [`test\ssa`](test/ssa) contains two test suites: `check` and `overhead`:
`check` uses [Turnt](https://github.com/cucapra/turnt) to test whether the SSA
conversion is indeed SSA. `overhead` runs the full to SSA - from SSA roundtrip
and measures the overhead of the conversion.

```
$> cd test/ssa/check
$> turnt *.bril
1..6
ok 1 - argwrite.bril
ok 2 - if-const.bril
ok 3 - if.bril
ok 4 - loop.bril
ok 5 - selfloop.bril
ok 6 - while.bril
```

The average overhead of SSA roundtrip for these small test programs is 34%.

```
$> cd test/ssa/overhead
$> brench brench.toml
benchmark,run,result
if,baseline,5
if,ssa,7
if,roundtrip,7
loop,baseline,26
loop,ssa,31
loop,roundtrip,31
selfloop,baseline,20
selfloop,ssa,25
selfloop,roundtrip,26
argwrite,baseline,4
argwrite,ssa,6
argwrite,roundtrip,7
while,baseline,34
while,ssa,41
while,roundtrip,41
if-const,baseline,5
if-const,ssa,6
if-const,roundtrip,6
```
