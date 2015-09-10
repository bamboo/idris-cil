# idris-cil [![Build Status](https://travis-ci.org/bamboo/idris-cil.png?branch=master)](https://travis-ci.org/bamboo/idris-cil)

A Common Intermediate Language backend for [Idris](http://http://www.idris-lang.org/).

## Why

Because programmers targeting the CLR deserve a modern pure functional language with dependent types.

## Building

The build currently depends on a patched version of [language-cil](https://github.com/tomlokhorst/language-cil).

```
git clone git@github.com:bamboo/language-cil --branch idris-cil --single-branch language-cil
cd language-cil
cabal sandbox init --sandbox ../sandbox
cabal install --only-dependencies
cabal install
cd ..

git clone git@github.com:bamboo/idris-cil
cd idris-cil
cd rts && idris --install cil.ipkg && cd ..
cabal sandbox init --sandbox ../sandbox
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests
cabal test
cabal install
```

## Usage

Given a `Main.idr` file:

```idris
module Main

main : IO ()
main = putStrLn "Hello, Idris!"
```

Compile it to cil and execute it with mono:

```
idris --codegen cil Main.idr -o HelloWorld.exe \
  && mono HelloWorld.exe
```

## TODO

* [ ] FFI: enums
* [ ] FFI: exception handling
* [ ] FFI: callbacks/delegates
* [ ] FFI: efficient arrays
* [ ] FFI: attach custom attributes to exported functions
* [ ] FFI: deal with `null` (detect null values from foreign invocations immediately)
* [ ] primitives: proper support for all integer types
* [ ] primitives: many more
* [x] FFI: read static fields
* [x] primitives: Double operators
* [x] FFI: foreign value types
* [x] FFI: array types
* [x] FFI: invoke instance methods
* [x] FFI: instantiate objects
* [x] FFI: invoke static methods
* [x] FFI: export functions as static methods
* [x] FFI: export functions including foreign types in their signatures
* [x] FFI: run IO on exported functions
* [x] FFI: pass `null` to foreign functions
* [x] primitives: Float
* [x] primitives: String
* [x] Tail calls
* [x] ADTs
