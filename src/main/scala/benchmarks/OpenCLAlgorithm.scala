package benchmarks

import benchmarks.OpenCLBenchmark.DpiaProgram
import idealised.OpenCL.{Kernel, ScalaFunction, `(`, `)=>`}
import idealised.SurfaceLanguage.{->, Expr}
import idealised.SurfaceLanguage.Types.{DataType, TypeInference}
import idealised.utils.{Time, TimeSpan}
import lift.arithmetic.ArithExpr

import scala.util.Random

sealed trait Correctness {
  final def isCorrect:Boolean = this == Correct

  def check():Unit = {
    println("Correctness check:")
    this match {
      case Correct => println("Correct!")
      case Wrong(wrongValue, wrongSize) =>
        if(wrongValue) {
          println("Value is wrong!")
        }
        wrongSize match {
          case None =>
          case Some(SizePair(actualOutputSize, expectedOutputSize)) =>
            println(s"Size is wrong! Expected $expectedOutputSize, found $actualOutputSize")
        }
    }
    assert(this.isCorrect)
  }
}

case class SizePair(actualOutputSize:Int, expectedOutputSize:Int)

case object Correct extends Correctness
final case class Wrong(wrongValue:Boolean, wrongSize:Option[SizePair]) extends Correctness

object Correctness {
  def apply(kernelOutput:Array[Float], scalaOutput:Array[Float]):Correctness = {
    if(kernelOutput.length == scalaOutput.length) {
      if(isSame(kernelOutput, scalaOutput)) Correct else Wrong(wrongValue = true, wrongSize = None)
    } else {
      val (kOut, sOut) = matchSize(kernelOutput, scalaOutput)
      val valueCorrect = isSame(kOut, sOut)
      Wrong(wrongValue = !valueCorrect, wrongSize = Some(SizePair(kernelOutput.length, scalaOutput.length)))
    }
  }

  private def isSame(a:Array[Float], b:Array[Float]):Boolean = {
    a.zip(b).forall{case (x,y) => Math.abs(x - y) < 0.01}
  }

  private def matchSize(a:Array[Float], b:Array[Float]):(Array[Float], Array[Float]) = {
    if(a.length > b.length) (a.take(b.length), b) else (a, b.take(a.length))
  }
}


trait OpenCLAlgorithm {
  //The Scala type representing the input data
  type Input
  //The scala type representing the final result of running the algorithm
  type Result

  private val verbose:Boolean = true

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

object OpenCLBenchmark {
  case class DpiaProgram(name:String, f:Expr[DataType -> DataType])
}

abstract class OpenCLBenchmark(val verbose:Boolean) {
  //The Scala type representing the input data
  type Input
  //The scala type representing the final result of running the algorithm
  type Result

  def dpiaPrograms:Seq[DpiaProgram]

  protected def makeInput(random:Random):Input

  def makeOutput(name:String, localSize:Int, globalSize:Int, code:String, runtimeMs:TimeSpan[Time.ms], correctness: Correctness):Result

  protected def runScalaProgram(input:Input):Array[Float]

  private def compile(dpiaProgram:DpiaProgram, localSize:ArithExpr, globalSize:ArithExpr):Kernel = {
    idealised.OpenCL.KernelGenerator.makeCode(TypeInference(dpiaProgram.f, Map()).toPhrase, localSize, globalSize)
  }

  final def run(localSize:Int, globalSize:Int):Seq[Result] = {
    opencl.executor.Executor.loadAndInit()

    val rand = new Random()
    val input = makeInput(rand)
    val scalaOutput = runScalaProgram(input)

    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}

    val results = for(dpiaProgram <- this.dpiaPrograms) yield {
      val kernel = this.compile(dpiaProgram, localSize, globalSize)
      val kernelFun = kernel.as[ScalaFunction`(`Input`)=>`Array[Float]]
      kernelFun(input `;`)
      val (kernelOutput, time) = kernelFun(input `;`)
      val correct = Correctness(kernelOutput, scalaOutput)
      if(verbose) {
        println(s"For program:${dpiaProgram.name}")
        println(s"DPIA code:${dpiaProgram.f}")
        println(s"Generated code:${kernel.code}")
      }
      makeOutput(dpiaProgram.name, localSize, globalSize, kernel.code, time, correct)
    }

    opencl.executor.Executor.shutdown()

    results
  }
}