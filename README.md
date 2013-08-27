There are two cases when you may need haskell-packages: if you are writing a
Haskell compiler (or any tool that processes Haskell source code, such as a
static analyzer), or if you want to integrate with an existing Haskell compiler
that uses haskell-packages.

## Writing a compiler

If you are writing a compiler, you typically want to integrate it with Cabal, to
be able to build ordinary Haskell packages.

If you go the hard way, this involves:

1. **Parsing command line parameters**. Sounds easy — just take a list of files to
    compile. In reality you also need to handle package ids and package dbs, CPP
    options (`-DFOO=1`), language extension flags (`-XRankNTypes`) etc.

    To integrate with Cabal, you also need to tell it the list of installed
    packages, supported languages and extensions etc.

2. Actual **integration with Cabal** means understanding how Cabal works and
    hard-coding support for your compiler. And then getting it accepted and
    waiting for the next Cabal release.

    You may pretend that you are GHC or other compiler that is already supported
    by Cabal. It might work, but often it won't, for various reasons. Also,
    GHC's command line protocol is quite complex.

3. **Package management**. You need to implement a package db mechanism, which would
    let you to keep track of installed packages. Then you'd have to implement a
    `ghc-pkg`-like tool to manage those databases, for both Cabal and your users.

**Or**, you can simply use this library!

It already has command line options parsing, Cabal support, and package
management. All you need to do is to provide the function to do actual
compilation and tell a couple of other things about your compiler. See
the `Distribution.HaskellSuite.Compiler` module for details.

## Using other compilers

Some compilers produce artifacts that you may want to use. A good example is the
`gen-iface` compiler from haskell-names which generates an *interface* for each
module — the set of all named entities (functions, types, classes) that are
exported by that module. This information can be then used either by other
compilers, or by tools like IDEs.

Assuming the compiler uses haskell-packages and exports its database type, it's
very easy to use its artifacts. See `Distribution.HaskellSuite.Packages` for
package resolution and `Distribution.HaskellSuite.Modules` for module
resolution.

## Installation

It doesn't matter what Cabal version you use together with haskell-packages, but
if you want to invoke a haskell-packages compiler from Cabal (see
[Usage](#usage)), you need to build our [fork of Cabal][Cabal]. Eventually it
will be merged into Cabal.

[Cabal]: https://github.com/feuerbach/Cabal

## Usage

To compile a Haskell package using a haskell-packages tool:

    cabal install --haskell-suite -w $TOOL

where `$TOOL` may be either a full path to the compiler, or just an executable
name if it's in `$PATH`.
