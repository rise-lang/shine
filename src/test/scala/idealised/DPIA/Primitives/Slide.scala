package idealised.DPIA.Primitives

import benchmarks.core.SimpleRunOpenCLProgram
import idealised.OpenCL.KernelWithSizes
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.{Expr, NatIdentifier}
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import idealised.utils.Time.ms
import idealised.utils.TimeSpan
import lift.arithmetic._

import scala.util.Random

class Slide extends idealised.util.Tests {

  test("Simple slide example should generate syntactic valid C code with two for loops") {
    val slideExample =
      nFun(n =>
        fun(ArrayType(n, float))(xs => xs :>> slide(3, 1) :>> mapSeq(mapSeq(fun(x => x)))))

    val p = idealised.C.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(slideExample, Map())))
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple 2D slide example with separate maps should generate syntactic valid OpenMP code with three for loops") {
    val slideExample =
      nFun(n => nFun(m =>
        fun(ArrayType(n, ArrayType(m, float)))( xs =>
          xs :>> map(slide(3, 1)) :>> mapSeq(mapSeq(mapSeq(fun(x => x)))) )))

    val p = idealised.OpenMP.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(slideExample, Map())))
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 3
  }

  test("Simple 2D slide example with merged maps should generate syntactic valid OpenMP code with three for loops") {
    val slideExample =
      nFun(n => nFun(m =>
        fun(ArrayType(n, ArrayType(m, float)))( xs =>
          xs :>> mapSeq(slide(3, 1) >>> mapSeq(mapSeq(fun(x => x)))) )))

    val p = idealised.OpenMP.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(slideExample, Map())))
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 3
  }

  test("Two dimensional slide test") {
    val slideSize = 3
    val slideStep = 1

    val f = nFun(n => nFun(m => fun(ArrayType(n, ArrayType(m, float)))(xs => xs  :>> slide2D(slideSize, slideStep) :>> mapSeq(fun(x => x)))))

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
}
