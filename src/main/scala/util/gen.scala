package util

import idealised.DPIA
import idealised.OpenCL.{GlobalSize, LocalSize}

object gen {
  private def toDPIA(e: lift.core.Expr): DPIA.Phrases.Phrase[_ <: DPIA.Types.PhraseType] = {
    val typed_e = lift.core.types.infer(e)
    idealised.DPIA.fromLift(typed_e)
  }

  def CProgram(e: lift.core.Expr, name: String = "foo"): idealised.C.Program = {
    val dpia_e = toDPIA(e)
    val p = idealised.C.ProgramGenerator.makeCode(dpia_e, name)
    SyntaxChecker(p.code)
    println(p.code)
    p
  }

  def OpenMPProgram(e: lift.core.Expr, name: String = "foo"): idealised.OpenMP.Program = {
    val dpia_e = toDPIA(e)
    val p = idealised.OpenMP.ProgramGenerator.makeCode(dpia_e, name)
    SyntaxChecker(p.code)
    println(p.code)
    p
  }

  def OpenCLKernel(e: lift.core.Expr, name: String = "foo"): idealised.OpenCL.KernelNoSizes = {
    val dpia_e = toDPIA(e)
    val p = idealised.OpenCL.KernelGenerator.makeCode(dpia_e, name)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
    p
  }

  def OpenCLKernel(localSize: LocalSize, globalSize: GlobalSize)
                  (e: lift.core.Expr, name: String): idealised.OpenCL.KernelWithSizes = {
    val dpia_e = toDPIA(e)
    val p = idealised.OpenCL.KernelGenerator.makeCode(localSize, globalSize)(dpia_e, name)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
    p
  }
}