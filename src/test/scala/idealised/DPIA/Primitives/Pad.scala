package idealised.DPIA.Primitives

import idealised.OpenCL.SurfaceLanguage.DSL.mapGlobal
import idealised.OpenMP.SurfaceLanguage.DSL.mapPar
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Semantics.FloatData
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import idealised.utils.ScalaPatterns
import lift.arithmetic._

import scala.util.Random

class Pad extends idealised.util.Tests {
  test("Simple C pad input and copy") {
    val f = fun(ArrayType(SizeVar("N"), float))( xs =>
      xs :>> pad(2, 3, 5.0f) :>> mapSeq(fun(x => x))
    )

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(f, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }

  test("Simple OpenMP pad input and copy") {
    val f = fun(ArrayType(SizeVar("N"), float))( xs =>
      xs :>> pad(2, 3, 5.0f) :>> mapPar(fun(x => x))
    )

    val p = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(f, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }

  test("Simple OpenCL pad input and copy") {
    val f = fun(ArrayType(SizeVar("N"), float))( xs =>
      xs :>> pad(2, 3, 5.0f) :>> mapGlobal(fun(x => x))
    )

    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase, ?, ?)
    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  test("OpenCL Pad only left") {
    val f = fun(ArrayType(SizeVar("N"), float))( xs =>
      xs :>> pad(2, 0, 5.0f) :>> mapGlobal(fun(x => x))
    )

    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase, ?, ?)
    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  test("OpenCL Pad only right") {
    val f = fun(ArrayType(SizeVar("N"), float))( xs =>
      xs :>> pad(0, 3, 5.0f) :>> mapGlobal(fun(x => x))
    )

    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase, ?, ?)
    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  test("Pad 2D (OpenCL)") {
    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}
    val N = SizeVar("N")
    val M = SizeVar("M")

    val padAmount = 2
    val padValue = 0.0f

    val f = fun(ArrayType(N, ArrayType(M, float)))(xs => xs :>> pad2D(M, Cst(padAmount), Cst(padAmount), FloatData(padValue)) :>> mapSeq(mapSeq(fun(x => x))))


    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase, localSize = 1, globalSize = 1)
    val kernelF = p.as[ScalaFunction`(`Array[Array[Float]]`)=>`Array[Float]]
    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)

    opencl.executor.Executor.loadAndInit()
    val random = new Random()
    val actualN = 2
    val actualM = 2
    val input = Array.fill(actualN)(Array.fill(actualM)(random.nextFloat()))
    val scalaOutput = ScalaPatterns.pad2D(input, padAmount, 0.0f).flatten

    val (kernelOutput, _) = kernelF(input `;`)
    opencl.executor.Executor.shutdown()

    assert(kernelOutput sameElements scalaOutput)
  }
}
