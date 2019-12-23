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

