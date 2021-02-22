package compiler

import shine.DPIA.Phrases.Phrase
import util.compiler._
import util.compiler.DSL._
import util.compiler.PartialCompiler._
import shine._

// examples of using the compiler forest API
class partialCompiler extends test_util.Tests {
  def pcLeftIdentity[S, T, S_, T_]: PartialCompiler[S, T, S_, T_] => PartialCompiler[S, T, S_, T_] =
    pc => idPC[S, T] composeWithPC (pc: PartialCompiler[S, T, S_, T_])

  def pcRightIdentity[S, T, S_, T_]: PartialCompiler[S, T, S_, T_] => PartialCompiler[S, T, S_, T_] =
    (pc: PartialCompiler[S, T, S_, T_]) => pc composeWithPC idPC[S_, T_]

  def cIdentity[S, T, S_, T_]: Compiler[S, T] => Compiler[S, T] =
    c => idPC[S, T] composeWith c

  def cTensorTest[S, T]: Compiler[S, T] => Compiler[(S, S), (T, T)] = (c: Compiler[S, T]) =>
    c x c

  def composedCompiler[S, T](decomp: S => Seq[S],
                             comp: Seq[T] => T,
                             c: Compiler[S, T]): Compiler[S, T] =
    seqPC(decomp, comp) composeWith map(c)

  def cpu_gpu_compiler[S, CPU_CODE, GPU_CODE](gpu_compiler: Compiler[S, GPU_CODE],
                                              cpu_compiler: Compiler[S, CPU_CODE],
                                              select_gpu_source: S => S,
                                              select_cpu_source: S => S,
                                              comb: (CPU_CODE, CPU_CODE) => CPU_CODE,
                                              embed_in_cpu_code: GPU_CODE => CPU_CODE,
                                             ): Compiler[S, CPU_CODE] = {
    binaryPC(
      select_gpu_source,
      select_cpu_source,
      comb
    ) composeWith (
        gpu_compiler andThen embed_in_cpu_code
      x
        cpu_compiler
      )
  }

  object DPIA {
    type C_Compiler = Compiler[Phrase[_], C.Module]
    type OpenCL_Compiler = Compiler[Phrase[_], OpenCL.KernelModule]
  }
}
