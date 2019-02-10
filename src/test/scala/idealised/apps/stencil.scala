package idealised.apps

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._

class stencil extends idealised.util.Tests {

  val N = SizeVar("N")
  val xsT = ArrayType(N, float)

  private val simpleStencil = fun(xsT)(xs =>
    xs :>> slide(3, 1) :>> mapSeq(fun(nbh =>
      nbh :>> reduceSeq(fun(x => fun(a => x + a)), 0.0f)
    ))
  )

  test("Simple stencil compiles to syntactically correct C") {
    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(simpleStencil, Map()).toPhrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Simple scan compiles to syntactically correct OpenMP") {
    val p = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(simpleStencil, Map()).toPhrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Simple scan compiles to syntactically correct OpenCL") {
    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(simpleStencil, Map()).toPhrase, ?, ?)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }

}
