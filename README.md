### Overview
This repository contains an implementation of the RISE language and its Shine compiler (https://rise-lang.org/).

The source code for the compiler is organised into sub-packages of the `shine` package.

### Setup, File Structure and Documentation
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