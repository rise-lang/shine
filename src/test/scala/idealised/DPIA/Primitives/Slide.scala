package idealised.DPIA.Primitives

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.NatIdentifier
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
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
    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}
    opencl.executor.Executor.loadAndInit()

    val slideSize = 3
    val slideStep = 1

    val f = nFun(n => nFun(m => fun(ArrayType(n, ArrayType(m, float)))(xs => xs  :>> slide2D(slideSize, slideStep) :>> mapSeq(fun(x => x)))))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(1,1)(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))
    val kernelF = kernel.as[ScalaFunction`(` Int `,` Int `,` Array[Array[Float]]`)=>`Array[Float]]

    val random = new Random()
    val actualN = 9
    val actualM = 6
    val input = Array.fill(actualN)(Array.fill(actualM)(random.nextFloat()))

    val (kernelOutput, _) = kernelF(actualN `,` actualM `,` input)

    val scalaOutput = input.map(_.sliding(slideSize, slideStep).toArray).sliding(slideSize, slideStep).toArray.map(_.transpose).flatten.flatten.flatten


    println(kernel.code)
    opencl.executor.Executor.shutdown()

    assert(kernelOutput sameElements scalaOutput)
  }
}
