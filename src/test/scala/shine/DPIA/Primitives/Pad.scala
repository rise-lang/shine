package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types._
import rise.core.HighLevelConstructs.padClamp2D

import util.gen

class Pad extends shine.test_util.Tests {
  private val id = fun(x => x)

  test("Simple C constant pad input and copy") {
    val e = nFun(n => fun(ArrayType(n, f32))(xs =>
      xs |> padCst(2)(3)(l(5.0f)) |> mapSeq(fun(x => x))
    ))

    gen.CProgram(e)
  }

  test("Simple C clamp pad input and copy") {
    val e = nFun(n => fun(ArrayType(n, f32))(xs =>
      xs |> padClamp(2)(3) |> mapSeq(fun(x => x))
    ))

    gen.CProgram(e)
  }

  test("2D C clamp pad input and copy") {
    val e = nFun(n => nFun(m => fun(ArrayType(n, ArrayType(m, f32)))(xs =>
      xs |> padClamp2D(2) |> mapSeq(mapSeq(fun(x => x)))
    )))

    gen.CProgram(e)
  }

  test("Simple OpenMP constant pad input and copy") {
    import rise.OpenMP.DSL._

    val e = nFun(n => fun(ArrayType(n, f32))( xs =>
      xs |> padCst(2)(3)(l(5.0f)) |> mapPar(fun(x => x))
    ))

    gen.OpenMPProgram(e)
  }

  test("Simple OpenCL pad input and copy") {
    import rise.OpenCL.DSL._

    val e = nFun(n => fun(ArrayType(n, f32))( xs =>
      xs |> padCst(2)(3)(l(5.0f)) |> mapGlobal(fun(x => x))
    ))

    gen.OpenCLKernel(e)
  }

  test("OpenCL Pad only left") {
    import rise.OpenCL.DSL._

    val e = nFun(n => fun(ArrayType(n, f32))( xs =>
      xs |> padCst(2)(0)(l(5.0f)) |> mapGlobal(fun(x => x))
    ))

    gen.OpenCLKernel(e)
  }

  test("OpenCL Pad only right") {
    import rise.OpenCL.DSL._

    val e = nFun(n => fun(ArrayType(n, f32))( xs =>
      xs |> padCst(0)(3)(l(5.0f)) |> mapGlobal(fun(x => x))
    ))

    gen.OpenCLKernel(e)
  }

  test("OpenCL pad before or after transpose") {
    import rise.OpenCL.DSL._

    val range = arithexpr.arithmetic.RangeAdd(1, arithexpr.arithmetic.PosInf, 1)
    val k1 = gen.OpenCLKernel(nFun(range, n =>
      fun((4`.`n`.`int) ->: ((n+2)`.`4`.`int))(xs =>
        xs |> transpose |> padClamp(1)(1) |> mapGlobal(mapSeqUnroll(id))
      )
    ))
    val k2 = gen.OpenCLKernel(nFun(range, n =>
      fun((4`.`n`.`int) ->: ((n+2)`.`4`.`int))(xs =>
        xs |> map(padClamp(1)(1)) |> transpose |> mapGlobal(mapSeqUnroll(id))
      )
    ))

    val N = 20
    val random = new scala.util.Random()
    val input = Array.fill(4, N)(random.nextInt)

    import shine.OpenCL._
    val localSize = LocalSize(1)
    val globalSize = GlobalSize(1)

    val f1 = k1.as[ScalaFunction `(` Int `,` Array[Array[Int]] `)=>`Array[Int]]
    val f2 = k2.as[ScalaFunction `(` Int `,` Array[Array[Int]] `)=>`Array[Int]]

    val ((r1, _), (r2, _)) = util.withExecutor {
      (f1(localSize, globalSize)(N `,` input),
        f2(localSize, globalSize)(N `,` input))
    }
    util.assertSame(r1, r2, "results are different")
  }

  /* TODO
    test("Pad 2D (OpenCL)") {
      val padAmount = 2
      val padValue = 0.0f

      val f = nFun(
        n => nFun(m =>
          fun(ArrayType(n, ArrayType(m, f32)))(xs => xs :>> pad2D(m, Cst(padAmount), Cst(padAmount), FloatData(padValue)) :>> mapSeq(mapSeq(fun(x => x))))
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
