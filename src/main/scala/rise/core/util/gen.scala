package rise.core.util

import rise.core.exprs.Expr
import rise.elevate.rules.traversal.default
import shine.DPIA
import shine.OpenCL.{GlobalSize, LocalSize}
import util.SyntaxChecker

object gen {
  private def toDPIA(e: Expr): DPIA.Phrases.Phrase[_ <: DPIA.Types.PhraseType] = {
    shine.DPIA.fromRise(e)(default.RiseTraversable)
  }

  def CProgram(e: Expr, name: String = "foo"): shine.C.Program = {
    val dpia_e = toDPIA(e)
    val p = shine.C.ProgramGenerator.makeCode(dpia_e, name)
    SyntaxChecker(p.code)
    println(p.code)
    p
  }

  def OpenMPProgram(e: Expr, name: String = "foo"): shine.OpenMP.Program = {
    val dpia_e = toDPIA(e)
    val p = shine.OpenMP.ProgramGenerator.makeCode(dpia_e, name)
    SyntaxChecker(p.code)
    println(p.code)
    p
  }

  def OpenCLKernel(e: Expr, name: String = "foo"): shine.OpenCL.KernelNoSizes = {
    val dpia_e = toDPIA(e)
    val p = shine.OpenCL.KernelGenerator.makeCode(dpia_e, name)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
    p
  }

  def OpenCLKernel(localSize: LocalSize, globalSize: GlobalSize)
                  (e: Expr, name: String): shine.OpenCL.KernelWithSizes = {
    OpenCLKernel(_ => (localSize, globalSize))(e, name)
  }

  def OpenCLKernel(localGlobalSize: DPIA.Phrases.Phrase[_ <: DPIA.Types.PhraseType] => (LocalSize, GlobalSize))
                  (e: Expr, name: String): shine.OpenCL.KernelWithSizes = {
    val dpia_e = toDPIA(e)
    val (localSize, globalSize) = localGlobalSize(dpia_e)
    val p = shine.OpenCL.KernelGenerator.makeCode(localSize, globalSize)(dpia_e, name)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
    p
  }
}
