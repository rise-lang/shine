package idealised.DPIA.Primitives

import idealised.OpenCL.SurfaceLanguage.DSL.mapGlobal
import idealised.OpenMP.SurfaceLanguage.DSL.mapPar
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.NatIdentifier
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._

class Pad extends idealised.util.Tests {
  test("Simple C pad input and copy") {
    val f = dFun((n: NatIdentifier) => fun(ArrayType(n, float))(xs =>
      xs :>> pad(2, 3, 5.0f) :>> mapSeq(fun(x => x))
    ))

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(f, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }

  test("Simple OpenMP pad input and copy") {
    val f = dFun((n: NatIdentifier) => fun(ArrayType(n, float))( xs =>
      xs :>> pad(2, 3, 5.0f) :>> mapPar(fun(x => x))
    ))

    val p = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(f, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }

  test("Simple OpenCL pad input and copy") {
    val f = dFun((n: NatIdentifier) => fun(ArrayType(n, float))( xs =>
      xs :>> pad(2, 3, 5.0f) :>> mapGlobal(fun(x => x))
    ))

    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase)
    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  test("OpenCL Pad only left") {
    val f = dFun((n: NatIdentifier) => fun(ArrayType(n, float))( xs =>
      xs :>> pad(2, 0, 5.0f) :>> mapGlobal(fun(x => x))
    ))

    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase)
    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  test("OpenCL Pad only right") {
    val f = dFun((n: NatIdentifier) => fun(ArrayType(n, float))( xs =>
      xs :>> pad(0, 3, 5.0f) :>> mapGlobal(fun(x => x))
    ))

    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase)
    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }
}
