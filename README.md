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
idris --codegen cil Main.idr -o HelloWorld.il \
  && ilasm HelloWorld.il \
  && mono HelloWorld.exe
```

## Command Line Options

```
idris-cil [.ibc] -o <output.(il|exe|dll)>
```
## TODO

* [ ] FFI: arrays
* [ ] FFI: exception handling
* [ ] FFI: callbacks/delegates
* [ ] FFI: attach custom attributes to exported functions
* [x] FFI: invoke instance methods
* [x] FFI: instantiate objects
* [x] FFI: invoke static methods
* [x] FFI: export functions as static methods
* [ ] primitives: many more
* [x] primitives: Float
* [ ] primitives: proper support for integers
* [x] primitives: String
* [x] Tail calls
* [x] ADTs
