package idealised.DPIA.Primitives

import idealised.SurfaceLanguage.{->, Expr}
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._
import opencl.executor.Executor.ExecutorFailureException

import scala.util.Random

class Slide extends idealised.util.Tests {

  test("Simple slide example should generate syntactic valid C code with two for loops") {
    val slideExample = fun(ArrayType(SizeVar("N"), float))(xs => xs :>> slide(3, 1) :>> mapSeq(mapSeq(fun(x => x))))

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple 2D slide example with separate maps should generate syntactic valid OpenMP code with three for loops") {
    val slideExample = fun(ArrayType(SizeVar("N"), ArrayType(SizeVar("M"), float)))( xs =>
      xs :>> map(slide(3, 1)) :>> mapSeq(mapSeq(mapSeq(fun(x => x)))) )

    val p = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 3
  }

  test("Simple 2D slide example with merged maps should generate syntactic valid OpenMP code with three for loops") {
    val slideExample = fun(ArrayType(SizeVar("N"), ArrayType(SizeVar("M"), float)))( xs =>
      xs :>> mapSeq(slide(3, 1) >>> mapSeq(mapSeq(fun(x => x)))) )

    val p = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 3
  }

  def slide2D(size:Int, step:Int,input:Array[Array[Float]]): Array[Float]= {
    input.map(_.sliding(size, step).toArray).sliding(size, step).toArray.map(_.transpose).flatten.flatten.flatten
  }
  def slide2D(size:ArithExpr, step:ArithExpr):Expr[DataType -> DataType] = {
    fun(xs => xs :>> map(slide(size,step)) :>> slide(size, step) :>> mapSeq(transpose))
  }

  test("Two dimensional slide test") {
    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}
    opencl.executor.Executor.loadAndInit()

    val N = NamedVar("N", StartFromRange(1))
    val M = NamedVar("M", StartFromRange(1))
    val f = fun(ArrayType(N, ArrayType(M, float)))(xs => xs  :>> slide2D(3, 1))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase, 1, 1)
    val kernelF = kernel.as[ScalaFunction`(`Array[Array[Float]]`)=>`Array[Float]]

    val random = new Random()
    val actualN = 9
    val actualM = 6
    val input = Array.fill(actualN)(Array.fill(actualM)(random.nextFloat()))

    val (kernelOutput, _) = kernelF(input `;`)

    val scalaOutput =  slide2D(3, 1, input)

    println(kernel.code)
    opencl.executor.Executor.shutdown()

    assert(kernelOutput sameElements scalaOutput)
  }
}
