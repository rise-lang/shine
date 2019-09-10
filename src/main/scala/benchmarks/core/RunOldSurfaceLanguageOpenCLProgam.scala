package benchmarks.core

import idealised.DPIA
import idealised.OpenCL.{GlobalSize, KernelWithSizes, LocalSize}
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Types.TypeInference
import idealised.utils.{Display, Time, TimeSpan}
import lift.arithmetic.ArithExpr

import scala.util.Random


abstract class RunOldSurfaceLanguageOpenCLProgam(val verbose:Boolean) {
  //The Scala type representing the input data
  type Input
  //The type of the summary structure recording data about the runs
  type Summary

  def expr: Expr

  protected def makeInput(random:Random):Input

  def makeSummary(localSize:Int, globalSize:Int, code:String, runtimeMs:Double, correctness: CorrectnessCheck):Summary

  protected def runScalaProgram(input:Input):Array[Float]

  private def compile(localSize:ArithExpr, globalSize:ArithExpr):KernelWithSizes = {
    val kernel = idealised.OpenCL.KernelGenerator.makeCode(LocalSize(localSize), GlobalSize(globalSize))(DPIA.FromSurfaceLanguage(TypeInference(this.expr, Map())), "kernel")

    if(verbose) {
      println(kernel.code)
    }
    kernel
  }

  protected def runKernel(k: KernelWithSizes, input: Input): (Array[Float], TimeSpan[Time.ms])

  final def run(localSize:Int, globalSize:Int):Summary = {
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
