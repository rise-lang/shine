package idealised.apps

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._

class scan extends idealised.util.Tests {

  private val xsT = ArrayType(8, float)
  private val mult = fun(x => x._1 * x._2)

  private val simpleScan = fun(xsT)(array => scanSeq(fun(x => fun(a => a + x)), 0.0f, array))

  test("Simple scan compiles to syntactically correct C") {
    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(simpleScan, Map()).toPhrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Simple scan compiles to syntactically correct OpenMP") {
    val p = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(simpleScan, Map()).toPhrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Simple scan compiles to syntactically correct OpenCL") {
    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(simpleScan, Map()).toPhrase, ?, ?)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }

}
