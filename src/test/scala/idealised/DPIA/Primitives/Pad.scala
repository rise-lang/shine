package idealised.DPIA.Primitives

import benchmarks.core.SimpleRunOpenCLProgram
import idealised.OpenCL.KernelWithSizes
import idealised.OpenCL.SurfaceLanguage.DSL.mapGlobal
import idealised.OpenMP.SurfaceLanguage.DSL.mapPar
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Semantics.FloatData
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import idealised.utils.{ScalaPatterns, TimeSpan}
import idealised.utils.Time.ms
import lift.arithmetic._

import scala.util.Random

class Pad extends idealised.util.Tests {
  test("Simple C pad input and copy") {
    val f = nFun(n => fun(ArrayType(n, float))(xs =>
      xs :>> pad(2, 3, 5.0f) :>> mapSeq(fun(x => x))
    ))

    val p = idealised.C.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }

  test("Simple OpenMP pad input and copy") {
    val f = nFun(n => fun(ArrayType(n, float))( xs =>
      xs :>> pad(2, 3, 5.0f) :>> mapPar(fun(x => x))
    ))

    val p = idealised.OpenMP.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }

  test("Simple OpenCL pad input and copy") {
    val f = nFun(n => fun(ArrayType(n, float))( xs =>
      xs :>> pad(2, 3, 5.0f) :>> mapGlobal(fun(x => x))
    ))

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))
    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  test("OpenCL Pad only left") {
    val f = nFun(n => fun(ArrayType(n, float))( xs =>
      xs :>> pad(2, 0, 5.0f) :>> mapGlobal(fun(x => x))
    ))

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))
    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  test("OpenCL Pad only right") {
    val f = nFun(n => fun(ArrayType(n, float))( xs =>
      xs :>> pad(0, 3, 5.0f) :>> mapGlobal(fun(x => x))
    ))

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))
    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  test("Pad 2D (OpenCL)") {
    val padAmount = 2
    val padValue = 0.0f

    val f = nFun(
      n => nFun(m =>
        fun(ArrayType(n, ArrayType(m, float)))(xs => xs :>> pad2D(m, Cst(padAmount), Cst(padAmount), FloatData(padValue)) :>> mapSeq(mapSeq(fun(x => x))))
      )
    )

    val actualN = 2
    val actualM = 2

    case class Run() extends SimpleRunOpenCLProgram(false) {
      override type Input = Array[Array[Float]]

      override def dpiaProgram: Expr = f

      override protected def makeInput(random: Random): Input = {
        Array.fill(actualN)(Array.fill(actualM)(random.nextFloat()))
      }

      override protected def runScalaProgram(input: Input): Array[Float] = {
        ScalaPatterns.pad2D(input, padAmount, 0.0f).flatten
      }

      override protected def runKernel(k: KernelWithSizes, input: Array[Array[Float]]): (Array[Float], TimeSpan[ms]) = {
        import idealised.OpenCL._

        val kernelFun = k.as[ScalaFunction `(` Int `,` Int `,` Input `)=>` Array[Float]]
        kernelFun(actualN `,` actualM `,` input)
      }
    }

    Run().run(localSize = 1, globalSize = 1).correctness.check()
  }
}
