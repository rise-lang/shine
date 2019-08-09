package apps

import benchmarks.core.{CorrectnessCheck, RunOpenCLProgram}
import idealised.OpenCL.{GlobalSize, KernelWithSizes, LocalSize}
import idealised.util.gen
import idealised.utils.Time.ms
import idealised.utils.{Display, TimeSpan}
import lift.OpenCL.primitives._
import lift.arithmetic.SteppedCase
import lift.core.DSL._
import lift.core.Expr
import lift.core.primitives._
import lift.core.types._
import lift.core.HighLevelConstructs._

import scala.util.Random

class stencil extends idealised.util.Tests {

  private case class StencilResult(inputSize: Int,
                                   stencilSize: Int,
                                   localSize: LocalSize,
                                   globalSize: GlobalSize,
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

    override def makeSummary(localSize: LocalSize, globalSize: GlobalSize, code: String, runtimeMs: Double, correctness: CorrectnessCheck): StencilResult = {
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

    val inputMinRange: Int = stencilSize //Used for `starts with` simplification

    final val padSize = stencilSize / 2
    println(stencilSize)
    assert(inputSize > inputMinRange)
    final override type Input = Array[Float]

    final def scalaProgram: Array[Float] => Array[Float] = (xs: Array[Float]) => {
      import idealised.utils.ScalaPatterns.pad
      pad(xs, padSize, 0.0f).sliding(stencilSize, 1).map(nbh => nbh.foldLeft(0.0f)(_ + _))
    }.toArray

    final override protected def makeInput(random: Random): Array[Float] = Array.fill(inputSize)(random.nextFloat())

    final override protected def runScalaProgram(input: Array[Float]): Array[Float] = scalaProgram(input)

  }

  private case class BasicStencil1D(inputSize: Int, stencilSize: Int) extends Stencil1DProgramRun {

    override def expr: Expr = {
      nFun(n => fun(ArrayType(n, float))(input =>
        input |>
          padCst(padSize)(padSize)(l(0.0f)) |>
          slide(stencilSize)(1) |>
          mapGlobal( oclReduceSeq(AddressSpace.Private)(add)(l(0.0f)) )
      ))
    }
  }

  private case class PartitionedStencil1D(inputSize: Int, stencilSize: Int) extends Stencil1DProgramRun {

    override def expr: Expr = {
      nFun(n => fun(ArrayType(n, float))(input =>
        input |>
          padCst(padSize)(padSize)(l(0.0f)) |>
          slide(stencilSize)(1) |>
          partition(3)(n2nFun(m => SteppedCase(m, Seq(padSize, n - 2 * padSize + ((1 + stencilSize) % 2), padSize)))) |>
          depMapSeq(mapGlobal(fun(nbh => oclReduceSeq(AddressSpace.Private)(add)(l(0.0f))(nbh)))) |>
          join
      ))
    }
  }

  private sealed trait Stencil2DProgramRun extends StencilBaseProgramRun {
    def inputSize: Int

    def stencilSize: Int

    final override type Input = Array[Array[Float]]

    protected val padSize = stencilSize / 2

    def expr: Expr

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
      fun(xs => xs |> join |> reduceSeq(add)(l(0.0f)))
    }
  }

  private case class BasicStencil2D(inputSize: Int, stencilSize: Int) extends Stencil2DProgramRun {
    override def expr = {
      nFun(n => fun(ArrayType(n, ArrayType(n, float)))(input =>
        input |>
          padCst2D(padSize)(padSize)(l(0.0f)) |>
          slide2D(stencilSize)(1) |>
          mapGlobal(1)(mapGlobal(0)(fun(nbh => join(nbh) |> oclReduceSeq(AddressSpace.Private)(add)(l(0.0f)))))
      )
      )
    }
  }

  private case class PartitionedStencil2D(inputSize: Int, stencilSize: Int) extends Stencil2DProgramRun {

    override def expr = {
      nFun(n => fun(ArrayType(n, ArrayType(n, float)))(input =>
        input |>
          padCst2D(padSize)(padSize)(l(0.0f)) |>
          slide2D(stencilSize)(1) |>
          //partition2D(padSize, N - 2*padSize + ((1 + stencilSize) % 2)) :>>
          partition(3)(n2nFun(m => SteppedCase(m, Seq(padSize, n - 2 * padSize, padSize)))) |>
          depMapSeq(
            //mapGlobal(0)(depMapSeqUnroll(mapGlobal(1)(join() >>> reduceSeq(add, 0.0f))))
            mapGlobal(1)(mapGlobal(0)(join >> oclReduceSeq(AddressSpace.Private)(add)(l(0.0f))))
          ) |>
          join
      ))
    }
  }

  private val simpleStencil = nFun(n => fun(ArrayType(n, float))(xs =>
    xs |> slide(3)(1) |> mapSeq(fun(nbh =>
      nbh |> reduceSeq(fun(x => fun(a => x + a)))(l(0.0f))
    ))
  ))

  test("Simple stencil compiles to syntactically correct C") {
    gen.CProgram(simpleStencil)
  }

  test("Simple scan compiles to syntactically correct OpenMP") {
    gen.OpenMPProgram(simpleStencil)
  }

  test("Basic 1D addition stencil") {
    BasicStencil1D(1024, 5).run(LocalSize(4), GlobalSize(4)).correctness.check()
  }

  ignore("Partitioned 1D addition stencil, with specialised area handling") {
    PartitionedStencil1D(256, 3).run(LocalSize(4), GlobalSize(32)).correctness.check()
  }

  test("Basic 2D addition stencil") {
    BasicStencil2D(256, stencilSize = 11).run(LocalSize(2), GlobalSize(4)).correctness.check()
  }

  ignore("Partitioned 2D addition stencil") {
    PartitionedStencil2D(inputSize = 1024, stencilSize = 11).run(LocalSize(4), GlobalSize(1024)).correctness.check()
  }
}
