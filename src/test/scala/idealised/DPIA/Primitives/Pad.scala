package idealised.DPIA.Primitives

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._

import idealised.util.gen

class Pad extends idealised.util.Tests {
  test("Simple C constant pad input and copy") {
    val e = nFun(n => fun(ArrayType(n, float))(xs =>
      xs |> padCst(2)(3)(l(5.0f)) |> mapSeq(fun(x => x))
    ))

    gen.CProgram(e)
  }

  test("Simple C clamp pad input and copy") {
    val e = nFun(n => fun(ArrayType(n, float))(xs =>
      xs |> padClamp(2)(3) |> mapSeq(fun(x => x))
    ))

    gen.CProgram(e)
  }

  test("Simple OpenMP constant pad input and copy") {
    import lift.OpenMP.primitives._

    val e = nFun(n => fun(ArrayType(n, float))( xs =>
      xs |> padCst(2)(3)(l(5.0f)) |> mapPar(fun(x => x))
    ))

    gen.OpenMPProgram(e)
  }

/* TODO
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

 */
}
