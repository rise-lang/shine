package benchmarks.core

import rise.elevate.rules.traversal.default.RiseTraversable
import shine.DPIA
import shine.OpenCL.{GlobalSize, KernelExecutor, LocalSize}
import rise.core.Expr
import util.{Display, Time, TimeSpan}

import scala.util.Random


abstract class RunOpenCLProgram(val verbose:Boolean) {
  //The Scala type representing the input data
  type Input
  //The type of the summary structure recording data about the runs
  type Summary

  def expr: Expr

  protected def makeInput(random:Random):Input

  def makeSummary(localSize:LocalSize, globalSize:GlobalSize, code:String, runtimeMs:Double, correctness: CorrectnessCheck):Summary

  protected def runScalaProgram(input:Input):Array[Float]

  private def compile(localSize:LocalSize, globalSize:GlobalSize):KernelExecutor.KernelWithSizes = {
    val ktu = util.gen.opencl.kernel.fromExpr("KERNEL", Some(localSize, globalSize))(this.expr)
    val kernel = shine.OpenCL.KernelExecutor.KernelWithSizes(ktu, localSize, globalSize)

    if(verbose) {
      println(kernel.code)
    }
    kernel
  }

  protected def runKernel(k: KernelExecutor.KernelWithSizes, input: Input): (Array[Float], TimeSpan[Time.ms])

  final def run(localSize:LocalSize, globalSize:GlobalSize):Summary = {
    opencl.executor.Executor.loadAndInit()

    val (scalaOutput, kernel, kernelOutput, time) = try {
      val rand = new Random()
      val input = makeInput(rand)
      val scalaOutput = runScalaProgram(input)

      val kernel = this.compile(localSize, globalSize)
      val (kernelOutput, time) = runKernel(kernel, input)
      (scalaOutput, kernel, kernelOutput, time)
    } finally {
      opencl.executor.Executor.shutdown()
    }

    val correct = CorrectnessCheck(kernelOutput, scalaOutput)

    makeSummary(localSize, globalSize, kernel.code, time.value, correct)
  }
}

abstract class SimpleRunOpenCLProgram(override val verbose: Boolean)
  extends RunOpenCLProgram(verbose) {

  final type Summary = Result

  case class Result(localSize: LocalSize,
                    globalSize: GlobalSize,
                    code: String,
                    runtimeMs: Double,
                    correctness: CorrectnessCheck
                   ) extends Display {
    def display: String =
      s"localSize = $localSize, " +
      s"globalSize = $globalSize, " +
      s"code = $code, " +
      s"runtime = $runtimeMs," +
      s" correct = ${correctness.display}"
  }

  override def makeSummary(localSize: LocalSize, globalSize: GlobalSize, code: String, runtimeMs: Double, correctness: CorrectnessCheck): Result =
    Result(localSize, globalSize, code, runtimeMs, correctness)
}
