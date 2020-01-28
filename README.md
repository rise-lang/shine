### Overview

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/efe2919980194123b990a9cc416f3eaf)](https://app.codacy.com/gh/rise-lang/shine?utm_source=github.com&utm_medium=referral&utm_content=rise-lang/shine&utm_campaign=Badge_Grade_Dashboard)

This repository contains an implementation of the Shine compiler for the RISE language
which is used as the main compiler of the RISE project (https://rise-lang.org/).

The source code for the compiler is organised into sub-packages of the `shine` package.

 - `DPIA` contains the implementation of Data Parallel Idealised Algol.
    The `Compilation` sub-package contains the implementation to rewrite a
    high-level functional program into an imperative representation. 
    Backend specific code generators for C, OpenMP and OpenCL are in their
    own packages.
    A paper describing DPIA is available at (https://arxiv.org/abs/1710.08332).
        
 - `C` contains the C backend printing an imperative DPIA representation into
    C code.
        
 - `OpenMP` contains the OpenMP backend printing an imperative DPIA
    representation into OpenMP code. The C backend is used to print most
    code.
    This package also defines extensions in form of additional patterns
    added to the RISE language and `DPIA`.
        
 - `OpenCL` contains the OpenCL backend printing an imperative DPIA
    representation into OpenCL code.
    This package also defines extensions in form of additional patterns
    added to the RISE language and `DPIA`.

### Setup
Prerequisits: Java 8+ SDK, `git`
1. Clone the `shine` repository to a directory of your choice that we call `$SHINE_ROOT`.
2. In `$SHINE_ROOT`, run `setup.sh` to download all required submodules.
3. Then run `sbt compile` (even if you want to use IDEA IntelliJ, because IntelliJ does not execute subproject dependencies correctly and the Executor needs to be built once in the beginning).

#### Configuring IntelliJ
4. Download IDEA IntelliJ (http://www.jetbrains.com/idea/) and choose to install the Scala Plugin during setup.
5. Launch IntelliJ. From the menu choose `File` -> `Open...`. Then, select `$SHINE_ROOT` and press `OK`.
6. From the menu bar choose `Run` -> `Edit Configurations...`. Under `Templates` choose `ScalaTest` and add the following line to `VM parameters`: `-Djava.library.path=lib/executor/lib/Executor/build -Xss16m` (this will tell IntelliJ where to find the Executor library and use 16MB of thread stack size which is needed for arithmetic simplifactions sometimes).
