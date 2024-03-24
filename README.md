### Overview
This repository contains an implementation of the RISE language and its Shine compiler (https://rise-lang.org/).

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

### Setup and Documentation
Please have a look at: https://rise-lang.org/doc/

### Float Safe Optimizer

This repository contains an optimizer executable that preserves floating-point semantics.
To build a Fat JAR executable:
```sh
sbt float_safe_optimizer/assembly
```
To optimize a Rise program and generate code:
```sh
java -Xss20m -Xms512m -Xmx4G -jar float-safe-optimizer.jar $function_name $rise_source_path $output_path
```
For example:
```sh
java -Xss20m -Xms512m -Xmx4G -jar float-safe-optimizer.jar add3 float-safe-optimizer/examples/add3Seq.rise float-safe-optimizer/examples/add3Seq.c
java -Xss20m -Xms512m -Xmx4G -jar float-safe-optimizer.jar add3 float-safe-optimizer/examples/add3.rise float-safe-optimizer/examples/add3.c
```