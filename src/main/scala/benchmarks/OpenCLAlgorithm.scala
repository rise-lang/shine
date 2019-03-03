package benchmarks

import benchmarks.core.Correctness
import idealised.OpenCL.Kernel
import idealised.SurfaceLanguage.Types.{DataType, TypeInference}
import lift.arithmetic.ArithExpr

import scala.util.Random



abstract class OpenCLAlgorithm(val verbose:Boolean) {
  import idealised.SurfaceLanguage._
  //The Scala type representing the input data
  type Input
  //The scala type representing the final result of running the algorithm
  type Result

  def dpiaProgram:Expr[DataType -> DataType]

  protected def makeInput(random:Random):Input

  def makeOutput(localSize:Int, globalSize:Int, code:String, runtimeMs:Double, correctness: Correctness):Result

  protected def runScalaProgram(input:Input):Array[Float]

  private def compile(localSize:ArithExpr, globalSize:ArithExpr):Kernel = {
    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(this.dpiaProgram, Map()).toPhrase, localSize, globalSize)

    if(verbose) {
      println(kernel.code)
    }
    kernel
  }

  final def run(localSize:Int, globalSize:Int):Result = {
    opencl.executor.Executor.loadAndInit()

    val rand = new Random()
    val input = makeInput(rand)
    val scalaOutput = runScalaProgram(input)

    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}

    val kernel = this.compile(localSize, globalSize)
    val kernelFun = kernel.as[ScalaFunction`(`Input`)=>`Array[Float]]
    val (kernelOutput, time) = kernelFun(input `;`)

    opencl.executor.Executor.shutdown()

    val correct = Correctness(kernelOutput, scalaOutput)

    makeOutput(localSize, globalSize, kernel.code, time.value, correct)
  }
}
