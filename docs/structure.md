---
id: structure
title: File Structure
sidebar_label: File Structure
---

## Rise & Shine Source Code

- `meta`: The source code for metaprogramming (macros).
- `runtime`: The source code for a lightweight runtime that Shine will target, in particular when mixing host and device code.
- `src/main/scala`:
  - `apps`: examples of applications
  - `benchmarks`: application benchmarks
  - `exploration`: autotuning related features
  - `rise`: source code related to the Rise language
    + `autotune`: autotuning related features
    + `elevate`: definition of rewriting strategies allowing precise control of the rewrite process. A paper describing Elevate is available at (https://doi.org/10.1145/3410227).
    + `eqsat`: equality saturation engine allowing powerful automation of the rewrite process. A paper describing equality saturation is available at (https://doi.org/10.1145/3434304). Guided Equality Saturation can be used to perform more complex optimizations (https://dl.acm.org/doi/10.1145/3632900).
    + `core`: core language features, independent of the hardware target
    + `OpenMP`: language features for OpenMP constructs
    + `OpenCL`: language features for OpenCL constructs
    + `Cuda`: language features for CUDA constructs
    + `GAP8`: language features for GAP8 architectures
  - `shine`: source code related to the Shine compiler
    + `DPIA`: implementation of Data Parallel Idealised Algol, the intermediate compilation language between functional Rise and imperative outputs. A paper describing DPIA is available at (https://arxiv.org/abs/1710.08332).
    + `C`: compilation to C code
    + `OpenMP`: compilation to OpenMP code, reusing parts of the C backend
    + `OpenCL`: compilation to OpenCL code
    + `cuda`: compilation to CUDA code
    + `GAP8`: compilation to GAP8 code
  - `util`: utilities
- `src/test/scala`: unit tests for the various Rise & Shine features

## Executables

- `float-safe-optimizer`: The source code for an optimizer executable built over Rise & Shine, that preserves floating-point semantics.

## Git Files

- `.github`: GitHub configuration files, including CI workflows
- `.gitignore`
- `.gitmodules`
- `lib`: Dependency submodules

## Scala Project Files

- `build.sbt`: Scala root configuration file
- `project`: Scala project configuration files
- `scalastyle_config.xlm`: configuration for Scala code style tools

## Other Files

- `data`: Raw data, including reference outputs for tests
- `docs`: The markdown code for these documentation pages
- `docs-website`: The source code for the documentation website
- `exploration`: Configuration files for autotuning examples