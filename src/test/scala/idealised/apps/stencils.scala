package idealised.apps

import idealised.OpenCL.Kernel
import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.DSL.{fun, _}
import idealised.SurfaceLanguage.Semantics.{FloatData, SingletonArrayData}
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{->, Expr, LiteralExpr}
import idealised.util.{Correctness, OpenCLAlgorithm, Tests}
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
    final type Output = StencilResult

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

    assert(inputSize > inputMinRange)
    final override type Input = Array[Float]

    def dpiaProgram:Expr[DataType -> DataType]

    final def scalaProgram: Array[Float] => Array[Float] = (xs:Array[Float]) => {
      val pad = Array.fill(padSize)(0.0f)
      val paddedXs = pad ++ xs ++ pad
      paddedXs.sliding(stencilSize, 1).map(nbh => nbh.foldLeft(0.0f)(_ + _))
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
          partition(3, m => SteppedCase(m, Seq(padSize + stencilSize, N - 2*stencilSize, padSize + stencilSize))) :>>
          depMapSeqUnroll(mapGlobal(fun(nbh => reduceSeq(add, 0.0f, nbh))))
      )
    }
  }


  private case class JustPadSlide2DAlgorithm(inputSize:Int, stencilSize:Int) extends StencilBaseAlgorithm {
    final override type Input = Array[Array[Float]]

    protected val padSize = stencilSize/2

    def dpiaProgram:Expr[DataType -> DataType] = {
      val N = NamedVar("N",StartFromRange(stencilSize))
      fun(ArrayType(N, ArrayType(N, float)))(input =>
        input :>>
          printType("Input") :>>
          idealised.SurfaceLanguage.DSL.pad2D(N, padSize, padSize, FloatData(0.0f)) :>>
          printType(s"Padded with $padSize") :>>
          idealised.SurfaceLanguage.DSL.slide2D(stencilSize, 1) :>> mapSeq(fun(x => x))
      )
    }


    final override protected def makeInput(random: Random): Array[Array[Float]] = Array.fill(inputSize)(Array.fill(inputSize)(random.nextFloat()))

    final override protected def runScalaProgram(input: Array[Array[Float]]): Array[Float] = scalaProgram(input).flatten

    private def pad2D(input:Array[Array[Float]]):Array[Array[Float]] = {
      val pad1D = Array.fill(padSize)(0.0f)
      val pad2D = Array.fill(padSize * 2 + inputSize)(0.0f)

      val data = Array(pad2D) ++ input.map(row => pad1D ++ row ++ pad1D) ++ Array(pad2D)
      data
    }

    private def slide2D(input:Array[Array[Float]]): Array[Array[Array[Array[Float]]]] = {
      input.map(_.sliding(stencilSize, 1).toArray).sliding(stencilSize, 1).map(x => x.transpose).toArray
    }

    final def scalaProgram: Array[Array[Float]] => Array[Array[Float]] = (grid:Array[Array[Float]]) => {
      slide2D(pad2D(grid)).flatten.flatten
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

    private def pad2D(input:Array[Array[Float]]):Array[Array[Float]] = {
      val pad1D = Array.fill(padSize)(0.0f)
      val pad2D = Array.fill(padSize * 2 + inputSize)(0.0f)

      val data = Array(pad2D) ++ input.map(row => pad1D ++ row ++ pad1D) ++ Array(pad2D)
      data
    }

    private def slide2D(input:Array[Array[Float]]): Array[Array[Array[Array[Float]]]] = {
      input.map(_.sliding(stencilSize, 1).toArray).sliding(stencilSize, 1).map(x => x.transpose).toArray
    }

    private def tileStencil(input:Array[Array[Float]]):Float = {
      input.flatten.reduceOption(_ + _).getOrElse(0.0f)
    }

    final def scalaProgram: Array[Array[Float]] => Array[Array[Float]] = (grid:Array[Array[Float]]) => {
      slide2D(pad2D(grid)).map(_.map(tileStencil))
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
          printType("Input") :>>
          pad2D(N, padSize, padSize, FloatData(0.0f)) :>>
          printType(s"Padded with $padSize") :>>
          slide2D(stencilSize, 1) :>>
          printType(s"Slided with $stencilSize") :>>
          mapGlobal(mapGlobal(fun(nbh => join(nbh) :>> reduceSeq(add, 0.0f))))
      )
    }
  }

  private case class PartitionedStencil2D(inputSize:Int, stencilSize:Int) extends Stencil2DAlgorithm {

    override def dpiaProgram = {
      val N = NamedVar("N",StartFromRange(stencilSize))
      fun(ArrayType(N, ArrayType(N, float)))(input =>
        input :>>
          printType("Input") :>>
          pad2D(N, padSize, padSize, FloatData(0.0f)) :>>
          printType(s"Padded with $padSize") :>>
          slide2D(stencilSize, 1) :>>
          printType(s"Slided with $stencilSize") :>>
          partition(3, m => SteppedCase(m, Seq(padSize + stencilSize, N - 2*stencilSize, padSize + stencilSize))) :>>
          printType("With outer partition") :>>
          depMapSeqUnroll(fun(inner =>
            inner :>>
              printType("Inner sizes") :>>
              partition(3, m => SteppedCase(m, Seq(padSize + stencilSize, N - 2*stencilSize, padSize + stencilSize))) :>>
              printType("After partition") :>>
              depMapSeqUnroll(mapGlobal(mapGlobal(fun(nbh => join(nbh) :>> reduceSeq(add, 0.0f)))))
          ))
      )
    }
  }

  test("Basic 1D addition stencil") {
    BasicStencil1D(1024, 5).run(localSize = 1, globalSize = 1).correctness.check()
  }

  test("Partitioned 1D addition stencil, with specialised area handling") {
    PartitionedStencil1D(1024, 5).run(localSize = 1, globalSize = 1).correctness.check()
  }

  test("Just 2D pad and slide") {
    JustPadSlide2DAlgorithm(9, 4).run(localSize = 1, globalSize = 1).correctness.check()
  }

  test("Basic 2D addition stencil") {
    BasicStencil2D(9, 3).run(localSize = 1, globalSize = 1).correctness.check()
  }

  test("Partitioned 2D addition stencil") {
    PartitionedStencil2D(128, 5).run(localSize = 1, globalSize = 1).correctness.check()
  }
}
