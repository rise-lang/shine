package idealised.apps

import benchmarks.{Correctness, OpenCLAlgorithm}
import idealised.OpenCL.Kernel
import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.DSL.{fun, _}
import idealised.SurfaceLanguage.Semantics.{FloatData, SingletonArrayData}
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{->, Expr, LiteralExpr}
import idealised.util.Tests
import lift.arithmetic._

import scala.util.Random

class stencils extends Tests {
  val add = fun(x => fun(y => x + y))


  private case class StencilResult(inputSize:Int,
                                     stencilSize:Int,
                                     localSize:Int,
                                     globalSize:Int,
                                     code:String,
                                     runtimeMs:Double,
                                     correctness:Correctness
                                    ) {

    def printout():Unit =
      println(
        s"inputSize = $inputSize, " +
        s"stencilSize = $stencilSize, " +
        s"localSize = $localSize, " +
        s"globalSize = $globalSize, " +
        s"code = $code, " +
        s"runtime = $runtimeMs," +
        s" correct = $correctness"
      )
  }

  private sealed trait StencilBaseAlgorithm extends OpenCLAlgorithm {
    final type Result = StencilResult

    def inputSize:Int
    def stencilSize:Int

    override def makeOutput(localSize: Int, globalSize: Int, code: String, runtimeMs: Double, correctness: Correctness): StencilResult = {
      StencilResult(
        inputSize = inputSize,
        stencilSize = stencilSize,
        localSize = localSize,
        globalSize = globalSize,
        code = code,
        runtimeMs = runtimeMs,
        correctness = correctness
      )
    }
  }

  private trait Stencil1DAlgorithm extends StencilBaseAlgorithm {

    val inputMinRange = stencilSize //Used for `starts with` simplification

    final val padSize = stencilSize/2
    println(stencilSize)
    assert(inputSize > inputMinRange)
    final override type Input = Array[Float]

    def dpiaProgram:Expr[DataType -> DataType]

    final def scalaProgram: Array[Float] => Array[Float] = (xs:Array[Float]) => {
      import idealised.utils.ScalaPatterns.pad
      pad(xs, padSize, 0.0f).sliding(stencilSize, 1).map(nbh => nbh.foldLeft(0.0f)(_ + _))
    }.toArray

    final override protected def makeInput(random: Random): Array[Float] = Array.fill(inputSize)(random.nextFloat())

    final override protected def runScalaProgram(input: Array[Float]): Array[Float] = scalaProgram(input)

  }

  private case class BasicStencil1D(inputSize:Int, stencilSize:Int) extends Stencil1DAlgorithm {

    override def dpiaProgram: Expr[DataType -> DataType] = {
      val N = NamedVar("N",StartFromRange(inputMinRange))
      fun(ArrayType(N, float))(input =>
        input :>> pad(padSize, padSize, 0.0f) :>> slide(stencilSize, 1) :>> mapGlobal(
          fun(nbh => reduceSeq(add, 0.0f, nbh)
          ))
      )
    }
  }

  private case class PartitionedStencil1D(inputSize:Int, stencilSize:Int) extends Stencil1DAlgorithm {

    override def dpiaProgram: Expr[DataType -> DataType] = {
      val N = NamedVar("N",StartFromRange(inputMinRange))
      fun(ArrayType(N, float))(input =>
        input :>> pad(padSize, padSize, 0.0f) :>>
          slide(stencilSize, 1) :>>
          partition(3, m => SteppedCase(m, Seq(padSize, N - stencilSize + 1, padSize))) :>>
          depMapSeqUnroll(mapGlobal(fun(nbh => reduceSeq(add, 0.0f, nbh)))) :>> printType()
      )
    }
  }

  private sealed trait Stencil2DAlgorithm extends StencilBaseAlgorithm {
    def inputSize:Int
    def stencilSize:Int

    final override type Input = Array[Array[Float]]

    protected val padSize = stencilSize/2
    def dpiaProgram:Expr[DataType -> DataType]


    final override protected def makeInput(random: Random): Array[Array[Float]] = Array.fill(inputSize)(Array.fill(inputSize)(random.nextFloat()))

    final override protected def runScalaProgram(input: Array[Array[Float]]) = scalaProgram(input).flatten


    private def tileStencil(input:Array[Array[Float]]):Float = {
      input.flatten.reduceOption(_ + _).getOrElse(0.0f)
    }

    final def scalaProgram: Array[Array[Float]] => Array[Array[Float]] = (grid:Array[Array[Float]]) => {
      import idealised.utils.ScalaPatterns._
      slide2D(pad2D(grid, padSize, 0.0f), stencilSize).map(_.map(tileStencil))
    }

    protected def tileStencil:Expr[DataType -> DataType] = {
      fun(xs => xs :>> join :>> reduceSeq(add, 0.0f))
    }
  }

  private case class BasicStencil2D(inputSize:Int, stencilSize:Int) extends Stencil2DAlgorithm {
    override def dpiaProgram = {
      val N = NamedVar("N",StartFromRange(stencilSize))
      fun(ArrayType(N, ArrayType(N, float)))(input =>
        input :>>
          pad2D(N, padSize, padSize, FloatData(0.0f)) :>>
          slide2D(stencilSize, 1) :>>
          mapGlobal(0)(mapGlobal(1)(fun(nbh => join(nbh) :>> reduceSeq(add, 0.0f))))
      )
    }
  }

  private case class PartitionedStencil2D(inputSize:Int, stencilSize:Int) extends Stencil2DAlgorithm {

    override def dpiaProgram = {
      val N = NamedVar("N",StartFromRange(1))
      fun(ArrayType(N, ArrayType(N, float)))(input =>
        input :>>
          pad2D(N, padSize, padSize, FloatData(0.0f)) :>>
          slide2D(stencilSize, 1) :>>
          partition2D(padSize, N - stencilSize + 1)
          :>> depMapSeqUnroll(fun(xs => xs :>> mapGlobal(0)(depMapSeqUnroll(mapGlobal(1)(fun(nbh => join(nbh) :>> reduceSeq(add, 0.0f)))))))
      )
    }
  }

  test("Basic 1D addition stencil") {
    BasicStencil1D(1024, 5).run(localSize = 4, globalSize = 4).correctness.check()
  }

  test("Partitioned 1D addition stencil, with specialised area handling") {
    PartitionedStencil1D(256, 6).run(localSize = 4, globalSize = 32).correctness.check()
  }

  test("Basic 2D addition stencil") {
    BasicStencil2D(128, 5).run(localSize = 4, globalSize = 32).correctness.check()
  }

  test("Partitioned 2D addition stencil") {
    PartitionedStencil2D(64, 5).run(localSize = 1, globalSize = 1).correctness.check()
  }
}
