package idealised.util

object gen {
  def CProgram(e: lift.core.Expr): idealised.C.Program = {
    val typed_e = lift.core.types.infer(e)
    val dpia_e = idealised.DPIA.fromLift(typed_e)
    val p = idealised.C.ProgramGenerator.makeCode(dpia_e)
    SyntaxChecker(p.code)
    println(p.code)
    p
  }
}