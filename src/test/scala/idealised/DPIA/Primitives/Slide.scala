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

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
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

    val p = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
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

    val p = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
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

    val N = NamedVar("N", StartFromRange(1))
    val M = NamedVar("M", StartFromRange(1))
    val f = fun(ArrayType(N, ArrayType(M, float)))(xs => xs  :>> slide2D(slideSize, slideStep) :>> mapSeq(fun(x => x)))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase, 1, 1)
    val kernelF = kernel.as[ScalaFunction`(`Array[Array[Float]]`)=>`Array[Float]]

    val random = new Random()
    val actualN = 9
    val actualM = 6
    val input = Array.fill(actualN)(Array.fill(actualM)(random.nextFloat()))

    val (kernelOutput, _) = kernelF(input `;`)

    val scalaOutput = input.map(_.sliding(slideSize, slideStep).toArray).sliding(slideSize, slideStep).toArray.map(_.transpose).flatten.flatten.flatten


    println(kernel.code)
    opencl.executor.Executor.shutdown()

    assert(kernelOutput sameElements scalaOutput)
  }
}
