This repository contains an implementation of DPIA (Data Parallel Idealised Algol)
which is used as the future foundation of the lift project (www.lift-project.org).

A paper describing DPIA is available at (https://arxiv.org/abs/1710.08332).

The source code is organised into sub-packages of the `idealised` package.

 - `SurfaceLanguage` contains an implementation of a sub-language intended
    as the highest level interface.
    This sub-language is purely functional and defines only algorithmic
    patterns. No patterns specific for a particular backend are defined here.   
         
 - `DPIA` contains the implementation of Data Parallel Idealised Algol.
    The `Compilation` sub-package contains the implementation to rewrite a
    high-level functional program into an imperative representation. 
    Backend specific code generators for C, OpenMP and OpenCL are in their
    own packages.
        
 - `C` contains the C backend printing an imperative DPIA representation into
    C code.
        
 - `OpenMP` contains the OpenMP backend printing an imperative DPIA
    representation into OpenMP code. The C backend is used to print most
    code.
    This package also defines extensions in form of additional patterns
    added to the `SurfaceLanguage` and `DPIA`.
        
 - `OpenCL` contains the OpenCL backend printing an imperative DPIA
    representation into OpenCL code.
    This package also defines extensions in form of additional patterns
    added to the `SurfaceLanguage` and `DPIA`.
