<p align="center">
  <img src="nox.png" alt="Nox" height="200"/>
<p>

# Overview

**Nox** is a specialized programming language designed to support the implementation of **data-oriented design (DOD)**.
Main inspirations are taken from _Odin_, _Rust_ and _Jai_.
Nox builds upon the different functionalities and concepts of those languages.

# Notable features

- Support for both **SoA** and **AoSoA** collections (arrays, dynamic arrays and slices)
- **Self-relative** pointer with proper copy semantics
- **Offset-relative** pointer as an alternative to regular pointer arithmetic
- **Implicit context** system which can be expanded upon
- The concept of **allocators** as an integral part of the language
- Differentiation between **procedures**, **functions** and **pure functions**

# Project structure

The project contains the following sub-projects:
- **nox-boot**: Contains the bootstrap compiler written in Odin
- **nox-lib**: Contains the packages of the standard library
- **nox-self**: Contains the self-hosted compiler written in Nox
- **nox-test**: Contains a simpled test-framework written in Nox to validate correct code generation
- **nox-vscode**: Contains an extension for Visual Studio Code which enables syntax highlighting for Nox
- **scripts**: Contains necessary build scripts
- **vendor**: Contains necessary dependencies for the bootstrap and self-hosted compiler

# Building

Currently the project differentiates between three versions of the compiler:
- **noxb**: The bootstrap compiler which got compiled with the _Odin_ compiler
- **noxs**: The self-hosted compiler which got compiled with the _noxb_ compiler
- **noxh**: The self-hosted compiler which got compiled with the _noxs_ compiler

## Scripts

The **scripts** folder contains the follwing build scripts:
- **build-noxb**: Compiles the _noxb_ compiler
- **build-noxs**: Compiles the _noxs_ compiler
- **build-noxh**: Compiles the _noxh_ compiler

The scripts are provided both as a `.bat` and `.sh` file to be executed on Windows and Linux respectively.

## Dependencies

The build script for the bootstrap compiler (noxb) requires the **Odin compiler** (version _dev-2023-05_) to be on the _path_.

### Windows

Windows SDK (usually part of Visual Studio) is required to properly link executables.

### Linux

The **Clang** compiler and the **LLVM project** (version 15.x) is required to build all compiler versions.
In particular the **llvm-config** tool must be accessible from the _path_.
