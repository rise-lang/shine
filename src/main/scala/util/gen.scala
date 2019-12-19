package util

import idealised.DPIA
import idealised.OpenCL.{GlobalSize, LocalSize}

object gen {
  private def toDPIA(e: rise.core.Expr): DPIA.Phrases.Phrase[_ <: DPIA.Types.PhraseType] = {
    val typed_e = rise.core.types.infer(e)
    idealised.DPIA.fromRise(typed_e)
  }

  def CProgram(e: rise.core.Expr, name: String = "foo"): idealised.C.Program = {
    val dpia_e = toDPIA(e)
    val p = idealised.C.ProgramGenerator.makeCode(dpia_e, name)
    SyntaxChecker(p.code)
    println(p.code)
    p
  }

  def OpenMPProgram(e: rise.core.Expr, name: String = "foo"): idealised.OpenMP.Program = {
    val dpia_e = toDPIA(e)
    val p = idealised.OpenMP.ProgramGenerator.makeCode(dpia_e, name)
    SyntaxChecker(p.code)
    println(p.code)
    p
  }

  def OpenCLKernel(e: rise.core.Expr, name: String = "foo"): idealised.OpenCL.KernelNoSizes = {
    val dpia_e = toDPIA(e)
    val p = idealised.OpenCL.KernelGenerator.makeCode(dpia_e, name)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
    p
  }

  def OpenCLKernel(localSize: LocalSize, globalSize: GlobalSize)
                  (e: rise.core.Expr, name: String): idealised.OpenCL.KernelWithSizes = {
    OpenCLKernel(_ => (localSize, globalSize))(e, name)
  }

  def OpenCLKernel(localGlobalSize: DPIA.Phrases.Phrase[_ <: DPIA.Types.PhraseType] => (LocalSize, GlobalSize))
                  (e: rise.core.Expr, name: String): idealised.OpenCL.KernelWithSizes = {
    val dpia_e = toDPIA(e)
    val (localSize, globalSize) = localGlobalSize(dpia_e)
    val p = idealised.OpenCL.KernelGenerator.makeCode(localSize, globalSize)(dpia_e, name)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
    p
  }
}