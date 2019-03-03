package idealised.apps

import benchmarks.OpenCLAlgorithm
import benchmarks.core.Correctness
import idealised.OpenCL.{Kernel, PrivateMemory}
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
        s" correct = ${correctness.printoutText}"
      )
  }

  private sealed abstract class StencilBaseAlgorithm extends OpenCLAlgorithm(verbose = false) {
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
        input :>>
          pad(padSize, padSize, 0.0f) :>>
          slide(stencilSize, 1) :>>
          mapGlobal(oclReduceSeq(add, 0.0f, PrivateMemory))
      )
    }
  }

  private case class PartitionedStencil1D(inputSize:Int, stencilSize:Int) extends Stencil1DAlgorithm {

    override def dpiaProgram: Expr[DataType -> DataType] = {
      val N = NamedVar("N",StartFromRange(inputMinRange))
      fun(ArrayType(N, float))(input =>
        input :>>
          pad(padSize, padSize, 0.0f) :>>
          slide(stencilSize, 1) :>>
          partition(3, m => SteppedCase(m, Seq(padSize,  N - 2*padSize + ((1 + stencilSize) % 2), padSize))) :>>
          depMapSeqUnroll(mapGlobal(fun(nbh => oclReduceSeq(add, 0.0f, PrivateMemory)(nbh)))) :>>
          join
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
          mapGlobal(1)(mapGlobal(0)(fun(nbh => join(nbh) :>> oclReduceSeq(add, 0.0f, PrivateMemory))))
      )
    }
  }

  private case class PartitionedStencil2D(inputSize:Int, stencilSize:Int) extends Stencil2DAlgorithm {

    override def dpiaProgram = {
      val N = NamedVar("N",StartFromRange(stencilSize))
      fun(ArrayType(N, ArrayType(N, float)))(input =>
        input :>>
          pad2D(N, padSize, padSize, FloatData(0.0f)) :>>
          slide2D(stencilSize, 1) :>>
            //partition2D(padSize, N - 2*padSize + ((1 + stencilSize) % 2)) :>>
          partition(3, m => SteppedCase(m, Seq(padSize, N - 2*padSize, padSize))) :>>
          depMapSeqUnroll(
            //mapGlobal(0)(depMapSeqUnroll(mapGlobal(1)(join() >>> reduceSeq(add, 0.0f))))
            mapGlobal(1)(mapGlobal(0)(join() >>> oclReduceSeq(add, 0.0f, PrivateMemory)))
          ) :>>
          join
      )
    }
  }

  private case class BasicStencil2DInjected(inputSize:Int, stencilSize:Int) extends Stencil2DAlgorithm {
    override def dpiaProgram = {
      val N = inputSize
      fun(ArrayType(N, ArrayType(N, float)))(input =>
        input :>>
          pad2D(N, padSize, padSize, FloatData(0.0f)) :>>
          slide2D(stencilSize, 1) :>>
          mapGlobal(0)(mapGlobal(1)(fun(nbh => join(nbh) :>> oclReduceSeq(add, 0.0f, PrivateMemory))))
      )
    }
  }

  private case class PartitionedStencil2DBoth(inputSize:Int, stencilSize:Int) extends Stencil2DAlgorithm {

    override def dpiaProgram = {
      val N =NamedVar("N", StartFromRange(stencilSize))
      val M = inputSize//NamedVar("M", StartFromRange(stencilSize))
      fun(ArrayType(N, ArrayType(M, float)))(input =>
        input :>>
          pad2D(M, padSize, padSize, FloatData(0.0f)) :>>
          slide2D(stencilSize, 1) :>>
          partition(3, m => SteppedCase(m, Seq(padSize, N - 2*padSize + 1, padSize))) :>>
          depMapSeqUnroll(
            mapGlobal(1)(fun(
              inner =>
                inner:>>
                  partition(3, m => SteppedCase(m, Seq(padSize, M-2*padSize + 1, padSize))) :>>
                  depMapSeqUnroll(mapGlobal(0)(join() >>> oclReduceSeq(add, 0.0f, PrivateMemory)))))
          ) :>> join
      )
    }
  }

  test("Basic 1D addition stencil") {
    BasicStencil1D(1024, 5).run(localSize = 4, globalSize = 4).correctness.check()
  }

  test("Partitioned 1D addition stencil, with specialised area handling") {
    PartitionedStencil1D(256, 3).run(localSize = 4, globalSize = 32).correctness.check()
  }

  test("Basic 2D addition stencil") {
    BasicStencil2D(256, stencilSize = 11).run(localSize = 2, globalSize = 4).correctness.check()
  }

  test("Partitioned 2D addition stencil") {
    PartitionedStencil2D(inputSize = 1024, stencilSize = 11).run(localSize = 4, globalSize = 1024).correctness.check()
  }

  test("Basic 2D  injected addition stencil") {
    BasicStencil2DInjected(8, stencilSize = 3).run(localSize = 2, globalSize = 4).correctness.check()
  }

  test("Partitioned 2D both dimensions addition stencil") {
    PartitionedStencil2DBoth(inputSize = 128, stencilSize = 6).run(localSize = 4, globalSize = 4).correctness.check()
  }
}
