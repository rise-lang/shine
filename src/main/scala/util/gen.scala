package util

import elevate.rise.rules.traversal.default.RiseTraversable
import shine.DPIA
import shine.OpenCL.{GlobalSize, LocalSize}

object gen {
  private def toDPIA(e: rise.core.Expr): DPIA.Phrases.Phrase[_ <: DPIA.Types.PhraseType] = {
    val typed_e = rise.core.types.infer(e)
    shine.DPIA.fromRise(typed_e)(RiseTraversable)
  }

  def CProgram(e: rise.core.Expr, name: String = "foo"): shine.C.Program = {
    val dpia_e = toDPIA(e)
    val p = shine.C.ProgramGenerator.makeCode(dpia_e, name)
    SyntaxChecker(p.code)
    //println(p.code)
    p
  }

  def OpenMPProgram(e: rise.core.Expr, name: String = "foo"): shine.OpenMP.Program = {
    val dpia_e = toDPIA(e)
    val p = shine.OpenMP.ProgramGenerator.makeCode(dpia_e, name)
    SyntaxChecker(p.code)
    //println(p.code)
    p
  }

  def OpenCLKernel(e: rise.core.Expr, name: String = "foo"): shine.OpenCL.KernelNoSizes = {
    val dpia_e = toDPIA(e)
    val p = shine.OpenCL.KernelGenerator.makeCode(dpia_e, name)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
    p
  }

  def OpenCLKernel(localSize: LocalSize, globalSize: GlobalSize)
                  (e: rise.core.Expr, name: String): shine.OpenCL.KernelWithSizes = {
    OpenCLKernel(_ => (localSize, globalSize))(e, name)
  }

  def OpenCLKernel(localGlobalSize: DPIA.Phrases.Phrase[_ <: DPIA.Types.PhraseType] => (LocalSize, GlobalSize))
                  (e: rise.core.Expr, name: String): shine.OpenCL.KernelWithSizes = {
    val dpia_e = toDPIA(e)
    val (localSize, globalSize) = localGlobalSize(dpia_e)
    val p = shine.OpenCL.KernelGenerator.makeCode(localSize, globalSize)(dpia_e, name)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
    p
  }
}