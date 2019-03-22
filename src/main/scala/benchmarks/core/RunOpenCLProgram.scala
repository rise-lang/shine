package benchmarks.core

import idealised.DPIA
import idealised.OpenCL.KernelWithSizes
import idealised.SurfaceLanguage.Types.TypeInference
import lift.arithmetic.ArithExpr

import scala.util.Random


abstract class RunOpenCLProgram(val verbose:Boolean) {
  import idealised.SurfaceLanguage._
  //The Scala type representing the input data
  type Input
  //The type of the summary structure recording data about the runs
  type Summary

  def inputSize:Int

  def dpiaProgram: Expr

  protected def makeInput(random:Random):Input

  def makeSummary(localSize:Int, globalSize:Int, code:String, runtimeMs:Double, correctness: CorrectnessCheck):Summary

  protected def runScalaProgram(input:Input):Array[Float]

  private def compile(localSize:ArithExpr, globalSize:ArithExpr):KernelWithSizes = {
    val kernel = idealised.OpenCL.KernelGenerator.makeCode(localSize, globalSize)(DPIA.FromSurfaceLanguage(TypeInference(this.dpiaProgram, Map())))

    if(verbose) {
      println(kernel.code)
    }
    kernel
  }

  final def run(localSize:Int, globalSize:Int):Summary = {
    opencl.executor.Executor.loadAndInit()

    val rand = new Random()
    val input = makeInput(rand)
    val scalaOutput = runScalaProgram(input)

    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}

    val kernel = this.compile(localSize, globalSize)
    val kernelFun = kernel.as[ScalaFunction`(`Int`,` Input`)=>`Array[Float]]
    val (kernelOutput, time) = kernelFun(inputSize `,` input)

    opencl.executor.Executor.shutdown()

    val correct = CorrectnessCheck(kernelOutput, scalaOutput)

    makeSummary(localSize, globalSize, kernel.code, time.value, correct)
  }
}
