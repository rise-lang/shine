package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import util.gen
import util.gen.c.function

class Slide extends test_util.Tests {

  test("Simple slide example should generate syntactic valid C code with two for loops") {
    val e =
      depFun((n: Nat) => fun(ArrayType(n, f32))(xs =>
        xs |> slide(3)(1) |> mapSeq(mapSeq(fun(x => x)))))

    val code = function.asStringFromExpr(e)

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple 2D slide example with separate maps should generate syntactic valid OpenMP code with three for loops") {
    val e =
      depFun((n: Nat, m: Nat) =>
        fun(ArrayType(n, ArrayType(m, f32)))( xs =>
          xs |> map(slide(3)(1)) |> mapSeq(mapSeq(mapSeq(fun(x => x))))
        ))

    val code = function.asStringFromExpr(e)

    "for".r.findAllIn(code).length shouldBe 3
  }

  test("Simple 2D slide example with merged maps should generate syntactic valid OpenMP code with three for loops") {
    val e =
      depFun((n: Nat, m: Nat) =>
        fun(ArrayType(n, ArrayType(m, f32)))( xs =>
          xs |> mapSeq(slide(3)(1) >> mapSeq(mapSeq(fun(x => x))))
        ))

    val code = function.asStringFromExpr(e)

    "for".r.findAllIn(code).length shouldBe 3
  }

  /* TODO
  test("Two dimensional slide test") {
    val slideSize = 3
    val slideStep = 1

    val f = depFun((n: Nat) => depFun((m: Nat) =>
      fun(ArrayType(n, ArrayType(m, f32)))(xs =>
        xs |> slide2D(slideSize, slideStep) |> mapSeq(fun(x => x)))))

    val actualN = 9
    val actualM = 6

    case class Run() extends SimpleRunOpenCLProgram(false) {
      override type Input = Array[Array[Float]]

      override def dpiaProgram: Expr = f

      override protected def makeInput(random: Random): Array[Array[Float]] = {
        Array.fill(actualN)(Array.fill(actualM)(random.nextFloat()))
      }

      override protected def runKernel(k: KernelWithSizes, input: Array[Array[Float]]): (Array[Float], TimeSpan[ms]) = {
        import idealised.OpenCL._

        val kernelFun = k.as[ScalaFunction `(` Int `,` Int `,` Input `)=>` Array[Float]]
        kernelFun(actualN `,` actualM `,` input)
      }

      override protected def runScalaProgram(input: Array[Array[Float]]): Array[Float] = {
        input.map(_.sliding(slideSize, slideStep).toArray).sliding(slideSize, slideStep).toArray.map(_.transpose).flatten.flatten.flatten
      }
    }

    Run().run(1, 1).correctness.check()
  }
  */
}
