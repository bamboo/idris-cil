# idris-cil [![Build Status](https://travis-ci.org/bamboo/idris-cil.png?branch=master)](https://travis-ci.org/bamboo/idris-cil)

A Common Intermediate Language backend for [Idris](http://www.idris-lang.org/).

## Why

Because programmers targeting the CLR deserve a modern pure functional language with dependent types.

## Usage

Given a `Main.idr` file:

```idris
module Main

main : IO ()
main = putStrLn "Hello, Idris!"
```

Compile it to cil and execute it with [.NET Core](https://www.microsoft.com/net/core):

```
idris --codegen cil Main.idr -o HelloWorld.exe \
  && dotnet HelloWorld.exe
```

The resulting assemblies can also be used with Mono or Unity.

## Installing

It's important that the version of the Idris executable matches the version used to build idris-cil:

	git clone git@github.com:bamboo/idris-cil
	cd idris-cil
	stack install idris
	stack install

## Contributing

Issue reports and PRs are welcome.

Brought to you by @bamboo, @sangamon and [contributors](https://github.com/bamboo/idris-cil/graphs/contributors).

## License

[![BSD3](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](LICENSE)

## Requirements

### .NET Core 1.1 with ilasm

Install `ilasm` using nuget:

    dotnet new console ilasm-setup
    cd ilasm-setup
    dotnet add package runtime.osx.10.10-x64.Microsoft.NETCore.ILAsm
    cd .. && rm -fr ilasm-setup

Replace the `runtime.osx.10.10-x64` prefix above with the right value for your system, for instance, `ubuntu.14.04-x64`.

Add `ilasm` to your `PATH` with something like:

    export PATH=$(find $HOME/.nuget/packages -name ilasm | xargs dirname):$PATH

If you get an error from `ilasm` saying it cannot load `libcoreclr.dylib`, add the `dotnet/shared` dir to `DYLD_LIBRARY_PATH`:

    export DYLD_LIBRARY_PATH=/usr/local/share/dotnet/shared/Microsoft.NETCore.App/1.1.2:$DYLD_LIBRARY_PATH

