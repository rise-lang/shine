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

### Setup
Prerequisits: Java 8+ SDK, `git`
1. Clone the `shine` repository to a directory of your choice that we call `$SHINE_ROOT`.
2. In `$SHINE_ROOT`, run `setup.sh` to download all required submodules.
3. Then run `sbt compile` (even if you want to use IDEA IntelliJ, because IntelliJ does not execute subproject dependencies correctly and the Executor needs to be built once in the beginning).

#### Configuring IntelliJ
4. Download IDEA IntelliJ (http://www.jetbrains.com/idea/) and choose to install the Scala Plugin during setup.
5. Launch IntelliJ. From the menu choose `File` -> `Open...`. Then, select `$SHINE_ROOT` and press `OK`.
6. From the menu bar choose `Run` -> `Edit Configurations...`. Under `Templates` choose `ScalaTest` and add the following line to `VM parameters`: `-Djava.library.path=lib/executor/lib/Executor/build -Xss26m` (this will tell IntelliJ where to find the Executor library and use 26MB of thread stack size which is needed for arithmetic simplifactions sometimes).

#### Dependencies and linking to OpenCL

##### On NixOS:

1. Add `pkgs.intel-ocl` to `hardware.opengl.extraPackages` and `nixos-rebuild`.
   Running `nix-shell -p clinfo --run clinfo` should show OpenCL support.
2. You will need `sbt`, `cmake` and `gcc` to compile the project: `nix-env -iA nixos.sbt nixos.cmake nixos.gcc`.
3. You will need `clang` to run some of the tests, however some utilities clash with `gcc`s, so we need to:
   ```
   nix-env --set-flag priority 15 gcc-wrapper # give gcc less priority
   nix-env -iA nixos.clang                    # install clang
   ```
4. Help `FindOpenCL` find the OpenCL library in NixOS:
   ```
   export LD_LIBRARY_PATH="${nix-build '<nixos>' --no-build-output ocl-icd}/lib/:$LD_LIBRARY_PATH"
   sbt compile
   ```
   (From now on `sbt compile` does not need to be told where the OpenCL bindings are.)
5. We can now run the tests with `sbt test`.

##### On Debian:

1. Install Portable OpenCL: `apt-get install pocl`
2. Install the build dependencies:
   ```
   apt-get install cmake software-properties-common g++ clang-7 libxtst6 libxrender1 libxext6
   ln -s /usr/bin/clang-7 /usr/bin/clang
   ln -s /usr/bin/clang-cpp-7 /usr/bin/clang++
   ```
3. Compile with `sbt compile` and test with `sbt test`.


##### As a Docker image

You can find the build file for a Docker image here:
https://github.com/michel-steuwer/docker-scala-opencl

Note that AMD's SDK is stored with git lfs, so you will have to download it after checking out the repository.
