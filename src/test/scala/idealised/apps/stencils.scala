package idealised.apps

import benchmarks.core.{CorrectnessCheck, RunOpenCLProgram}
import idealised.OpenCL.{KernelWithSizes, PrivateMemory}
import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.DSL.{fun, _}
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Semantics.FloatData
import idealised.SurfaceLanguage.Types._
import idealised.util.Tests
import idealised.utils.Time.ms
import idealised.utils.{Display, TimeSpan}
import lift.arithmetic._

import scala.util.Random
import scala.language.reflectiveCalls

class stencils extends Tests {
  val add = fun(x => fun(y => x + y))


  private case class StencilResult(inputSize: Int,
                                   stencilSize: Int,
                                   localSize: Int,
                                   globalSize: Int,
                                   code: String,
                                   runtimeMs: Double,
                                   correctness: CorrectnessCheck
                                  ) extends Display {

    def display: String =
      s"inputSize = $inputSize, " +
        s"stencilSize = $stencilSize, " +
        s"localSize = $localSize, " +
        s"globalSize = $globalSize, " +
        s"code = $code, " +
        s"runtime = $runtimeMs," +
        s" correct = ${correctness.display}"
  }

  private sealed abstract class StencilBaseProgramRun extends RunOpenCLProgram(verbose = false) {
    final type Summary = StencilResult

    def inputSize: Int

    def stencilSize: Int

    override def makeSummary(localSize: Int, globalSize: Int, code: String, runtimeMs: Double, correctness: CorrectnessCheck): StencilResult = {
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

    override protected def runKernel(k: KernelWithSizes, input: Input): (Array[Float], TimeSpan[ms]) = {
      import idealised.OpenCL._

      val kernelFun = k.as[ScalaFunction `(` Int `,` Input `)=>` Array[Float]]
      kernelFun(inputSize `,` input)
    }
  }

  private trait Stencil1DProgramRun extends StencilBaseProgramRun {

    val inputMinRange = stencilSize //Used for `starts with` simplification

    final val padSize = stencilSize / 2
    println(stencilSize)
    assert(inputSize > inputMinRange)
    final override type Input = Array[Float]

    def dpiaProgram: Expr

    final def scalaProgram: Array[Float] => Array[Float] = (xs: Array[Float]) => {
      import idealised.utils.ScalaPatterns.pad
      pad(xs, padSize, 0.0f).sliding(stencilSize, 1).map(nbh => nbh.foldLeft(0.0f)(_ + _))
    }.toArray

    final override protected def makeInput(random: Random): Array[Float] = Array.fill(inputSize)(random.nextFloat())

    final override protected def runScalaProgram(input: Array[Float]): Array[Float] = scalaProgram(input)

  }

  private case class BasicStencil1D(inputSize: Int, stencilSize: Int) extends Stencil1DProgramRun {

    override def dpiaProgram: Expr = {
      nFun(n => fun(ArrayType(n, float))(input =>
        input :>>
          pad(padSize, padSize, 0.0f) :>>
          slide(stencilSize, 1) :>>
          mapGlobal(oclReduceSeq(add, 0.0f, PrivateMemory))
      ))
    }
  }

  private case class PartitionedStencil1D(inputSize: Int, stencilSize: Int) extends Stencil1DProgramRun {

    override def dpiaProgram: Expr = {
      nFun(n => fun(ArrayType(n, float))(input =>
        input :>>
          pad(padSize, padSize, 0.0f) :>>
          slide(stencilSize, 1) :>>
          partition(3, m => SteppedCase(m, Seq(padSize, n - 2 * padSize + ((1 + stencilSize) % 2), padSize))) :>>
          depMapSeqUnroll(mapGlobal(fun(nbh => oclReduceSeq(add, 0.0f, PrivateMemory)(nbh)))) :>>
          join
      ))
    }
  }

  private sealed trait Stencil2DProgramRun extends StencilBaseProgramRun {
    def inputSize: Int

    def stencilSize: Int

    final override type Input = Array[Array[Float]]

    protected val padSize = stencilSize / 2

    def dpiaProgram: Expr

    final override protected def makeInput(random: Random): Array[Array[Float]] = Array.fill(inputSize)(Array.fill(inputSize)(random.nextFloat()))

    final override protected def runScalaProgram(input: Array[Array[Float]]) = scalaProgram(input).flatten


    private def tileStencil(input: Array[Array[Float]]): Float = {
      input.flatten.reduceOption(_ + _).getOrElse(0.0f)
    }

    final def scalaProgram: Array[Array[Float]] => Array[Array[Float]] = (grid: Array[Array[Float]]) => {
      import idealised.utils.ScalaPatterns._
      slide2D(pad2D(grid, padSize, 0.0f), stencilSize).map(_.map(tileStencil))
    }

    protected def tileStencil: Expr = {
      fun(xs => xs :>> join :>> reduceSeq(add, 0.0f))
    }
  }

  private case class BasicStencil2D(inputSize: Int, stencilSize: Int) extends Stencil2DProgramRun {
    override def dpiaProgram = {
      nFun(n => fun(ArrayType(n, ArrayType(n, float)))(input =>
        input :>>
          pad2D(n, padSize, padSize, FloatData(0.0f)) :>>
          slide2D(stencilSize, 1) :>>
          mapGlobal(1)(mapGlobal(0)(fun(nbh => join(nbh) :>> oclReduceSeq(add, 0.0f, PrivateMemory))))
      )
      )
    }
  }

  private case class PartitionedStencil2D(inputSize: Int, stencilSize: Int) extends Stencil2DProgramRun {

    override def dpiaProgram = {
      nFun(n => fun(ArrayType(n, ArrayType(n, float)))(input =>
        input :>>
          pad2D(n, padSize, padSize, FloatData(0.0f)) :>>
          slide2D(stencilSize, 1) :>>
          //partition2D(padSize, N - 2*padSize + ((1 + stencilSize) % 2)) :>>
          partition(3, m => SteppedCase(m, Seq(padSize, n - 2 * padSize, padSize))) :>>
          depMapSeqUnroll(
            //mapGlobal(0)(depMapSeqUnroll(mapGlobal(1)(join() >>> reduceSeq(add, 0.0f))))
            mapGlobal(1)(mapGlobal(0)(join() >>> oclReduceSeq(add, 0.0f, PrivateMemory)))
          ) :>>
          join
      ))
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
}
