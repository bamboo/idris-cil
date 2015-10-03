# idris-cil [![Build Status](https://travis-ci.org/bamboo/idris-cil.png?branch=master)](https://travis-ci.org/bamboo/idris-cil)

A Common Intermediate Language backend for [Idris](http://http://www.idris-lang.org/).

## Why

Because programmers targeting the CLR deserve a modern pure functional language with dependent types.

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

The resulting assemblies can also be used with .NET or Unity.

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

## Contributing

Issue reports and PRs are welcome.
