package apps

import benchmarks.core.{CorrectnessCheck, RunOpenCLProgram}
import shine.OpenCL.{GlobalSize, KernelExecutor, LocalSize}
import util.{Display, TimeSpan, gen}
import util.Time.ms
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq
import arithexpr.arithmetic.SteppedCase
import rise.core.DSL._
import rise.core.primitives._
import Type._
import rise.core.Expr
import rise.core.types._
import HighLevelConstructs._
import util.gen.c.function

import scala.util.Random

class stencil extends test_util.Tests {

  private case class StencilResult(
    inputSize: Int,
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

  sealed abstract private class StencilBaseProgramRun
    extends RunOpenCLProgram(verbose = false)
  {
    final type Summary = StencilResult

    def inputSize: Int

    def stencilSize: Int

    override def makeSummary(
      localSize: LocalSize,
      globalSize: GlobalSize,
      code: String,
      runtimeMs: Double,
      correctness: CorrectnessCheck
    ): StencilResult = {
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

    override protected def runKernel(
      k: KernelExecutor.KernelWithSizes,
      input: Input
    ): (Array[Float], TimeSpan[ms]) = {
      import shine.OpenCL._

      val kernelFun = k.as[ScalaFunction `(` Int `,` Input `)=>` Array[Float]]
      kernelFun(inputSize `,` input)
    }
  }

  private trait Stencil1DProgramRun extends StencilBaseProgramRun {
    // Used for `starts with` simplification
    val inputMinRange: Int = stencilSize

    final val padSize = stencilSize / 2
    logger.debug(stencilSize.toString)
    assert(inputSize > inputMinRange)
    final override type Input = Array[Float]

    final def scalaProgram: Array[Float] => Array[Float] =
      (xs: Array[Float]) => {
        import util.ScalaPatterns.pad
        pad(xs, padSize, 0.0f)
          .sliding(stencilSize, 1)
          .map(nbh => nbh.foldLeft(0.0f)(_ + _))
      }.toArray

    final override protected def makeInput(random: Random): Array[Float] =
      Array.fill(inputSize)(random.nextFloat())

    final override protected def runScalaProgram(
      input: Array[Float]
    ): Array[Float] = scalaProgram(input)

  }

  private case class BasicStencil1D(inputSize: Int, stencilSize: Int)
    extends Stencil1DProgramRun
  {
    override def expr: Expr = {
      depFun((n: Nat) => fun(ArrayType(n, f32))(input =>
        input |>
        padCst(padSize)(padSize)(lf32(0.0f)) |>
        slide(stencilSize)(1) |>
        mapGlobal(oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f)))
      ))
    }
  }

  private case class PartitionedStencil1D(inputSize: Int, stencilSize: Int)
    extends Stencil1DProgramRun
  {
    override def expr: Expr = {
      depFun((n: Nat) => fun(ArrayType(n, f32))(input =>
        input |>
        padCst(padSize)(padSize)(lf32(0.0f)) |>
        slide(stencilSize)(1) |>
        partition(3)(n2nFun(m =>
          SteppedCase(m,
            Seq(padSize, n - 2 * padSize + ((1 + stencilSize) % 2), padSize)
          )
        )) |>
        depMapSeq(mapGlobal(fun(nbh =>
          oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))(nbh)
        ))) |>
        join
      ))
    }
  }

  sealed private trait Stencil2DProgramRun extends StencilBaseProgramRun {
    def inputSize: Int

    def stencilSize: Int

    final override type Input = Array[Array[Float]]

    protected val padSize: Int = stencilSize / 2

    def expr: Expr

    final override protected def makeInput(
      random: Random
    ): Array[Array[Float]] =
      Array.fill(inputSize)(Array.fill(inputSize)(random.nextFloat()))

    final override protected def runScalaProgram(
      input: Array[Array[Float]]
    ): Array[Float] =
      scalaProgram(input).flatten

    private def tileStencil(input: Array[Array[Float]]): Float = {
      input.flatten.reduceOption(_ + _).getOrElse(0.0f)
    }

    final def scalaProgram: Array[Array[Float]] => Array[Array[Float]] =
      (grid: Array[Array[Float]]) => {
        import util.ScalaPatterns._
        slide2D(pad2D(grid, padSize, 0.0f), stencilSize).map(_.map(tileStencil))
      }

    protected def tileStencil: Expr = {
      fun(xs => xs |> join |> reduceSeq(add)(lf32(0.0f)))
    }
  }

  private case class BasicStencil2D(inputSize: Int, stencilSize: Int)
    extends Stencil2DProgramRun
  {
    override def expr: Expr = {
      depFun((n: Nat) => fun(ArrayType(n, ArrayType(n, f32)))(input =>
        input |>
        padCst2D(padSize)(lf32(0.0f)) |>
        slide2D(stencilSize, 1) |>
        mapGlobal(1)(mapGlobal(0)(fun(nbh =>
          join(nbh) |> oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))
        )))
      ))
    }
  }

  private case class PartitionedStencil2D(inputSize: Int, stencilSize: Int)
    extends Stencil2DProgramRun
  {

    override def expr: Expr = {
      depFun((n: Nat) => fun(ArrayType(n, ArrayType(n, f32)))(input =>
        input |>
        padCst2D(padSize)(lf32(0.0f)) |>
        slide2D(stencilSize, 1) |>
        // partition2D(padSize, N - 2*padSize + ((1 + stencilSize) % 2)) :>>
        partition(3)(n2nFun(m =>
          SteppedCase(m, Seq(padSize, n - 2 * padSize, padSize))
        )) |>
        depMapSeq(
          // mapGlobal(0)(depMapSeqUnroll(mapGlobal(1)(join() >>> reduceSeq(add, 0.0f))))
          mapGlobal(1)(mapGlobal(0)(
            join >> oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))
          ))
        ) |>
        join
      ))
    }
  }

  private val simpleStencil = depFun((n: Nat) => fun(ArrayType(n, f32))(xs =>
    xs |> slide(3)(1) |> mapSeq(fun(nbh =>
      nbh |> reduceSeq(fun(a => fun(x => a + x)))(lf32(0.0f))
    ))
  ))

  test("Simple stencil compiles to syntactically correct C") {
    function.asStringFromExpr(simpleStencil)
  }

  test("Simple scan compiles to syntactically correct OpenMP") {
    gen.openmp.function.asStringFromExpr(simpleStencil)
  }

  test("Basic 1D addition stencil") {
    BasicStencil1D(1024, 5).run(LocalSize(4), GlobalSize(4)).correctness.check()
  }

  ignore("Partitioned 1D addition stencil, with specialised area handling") {
    PartitionedStencil1D(256, 3)
      .run(LocalSize(4), GlobalSize(32))
      .correctness
      .check()
  }

  test("Basic 2D addition stencil") {
    BasicStencil2D(256, stencilSize = 11)
      .run(LocalSize(2), GlobalSize(4))
      .correctness
      .check()
  }

  ignore("Partitioned 2D addition stencil") {
    PartitionedStencil2D(inputSize = 1024, stencilSize = 11)
      .run(LocalSize(4), GlobalSize(1024))
      .correctness
      .check()
  }
}
