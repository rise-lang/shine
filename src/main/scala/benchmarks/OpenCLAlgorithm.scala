package benchmarks

import benchmarks.OpenCLBenchmark.{DpiaProgram, Parameter}
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
    println(this.printoutText)
    assert(this.isCorrect)
  }

  def printoutText:String = {
    def wrongValueText(wrongValue:Boolean):String = if(wrongValue) { "Values are wrong;" } else ""
    def wrongSizeText(sp: Option[SizePair]):String = sp.map(
      {
        case SizePair(actual, expected) => s"Size wrong: expected $expected but $actual found;"
      }).getOrElse("")

    this match {
      case Correct => "Correct"
      case Wrong(wrongValue, wrongSize) => wrongValueText(wrongValue) ++ wrongSizeText(wrongSize)

    }
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
  import idealised.SurfaceLanguage._
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
  case class Parameter(name:String, values:Seq[Int])

  def makeSpace(params:List[Parameter]):Seq[Map[String,Int]] = {
    params match {
      case Nil => Seq(Map())
      case x::xs =>
        makeSpace(xs).flatMap(valueMap => x.values.map(value => valueMap + (x.name -> value)))
    }
  }

  case class DpiaProgram(name:String, makeProgram:Map[String,Int] => Expr[DataType -> DataType])
}

abstract class OpenCLBenchmark(val verbose:Boolean, val runsPerProgram:Int) {
  //The Scala type representing the input data
  type Input
  //The scala type representing the final result of running the algorithm
  type Result

  def dpiaPrograms:Seq[DpiaProgram]

  protected def makeInput(inputSize:Int, random:Random):Input

  def makeOutput(name:String, paramMap:Map[String,Int], inputSize:Int, localSize:Int, globalSize:Int, code:String, runtimeMs:TimeSpan[Time.ms], correctness: Correctness):Result

  def resultPrintout(result:Result):Option[String] = None

  protected def runScalaProgram(input:Input, params:Map[String,Int]):Array[Float]

  private def compile(dpiaProgram:Expr[DataType -> DataType], localSize:ArithExpr, globalSize:ArithExpr):Kernel = {
    idealised.OpenCL.KernelGenerator.makeCode(TypeInference(dpiaProgram, Map()).toPhrase, localSize, globalSize)
  }

  final def explore(inputSize:Parameter, localSizeParam:Parameter, customParams:Set[Parameter]):Seq[Result] = {
    OpenCLBenchmark.makeSpace(customParams.toList).flatMap(run(inputSize, localSizeParam, _))
  }



  final def run(inputSizeParam:Parameter,
                localSizeParam:Parameter,
                paramMap:Map[String, Int]):Seq[Result] = {
    inputSizeParam.values.flatMap(inputSize => {
      val rand = new Random()
      val input = makeInput(inputSize, rand)
      val scalaOutput = runScalaProgram(input, paramMap)
      val globalSize = inputSize
      localSizeParam.values.flatMap(localSize => {
        val configuration = Configuration(inputSize, localSize, globalSize, paramMap)
        println("Running configuration")
        configuration.printOut()
        run(input, scalaOutput, configuration)
      })
    })
  }

  case class Configuration(inputSize:Int, localSize:Int, globalSize:Int, paramMap:Map[String,Int]) {
    def printOut(): Unit = {
      println(s"{ inputSize = $inputSize, localSize = $localSize, globalSize = $globalSize, paramMap = $paramMap })")
    }
  }

  private def run(input:Input,
                  scalaOutput:Array[Float],
                  conf: Configuration):Seq[Result] = {


    opencl.executor.Executor.loadAndInit()

    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}

    val results = for(dpiaProgram <- this.dpiaPrograms) yield {
      val tenRuns = for(runNum <- 0 until runsPerProgram) yield {
        print(s"$runNum: Running '${dpiaProgram.name}'")
        val dpiaSource = dpiaProgram.makeProgram(conf.paramMap)
        val kernel = this.compile(dpiaSource, conf.localSize, conf.globalSize)
        val kernelFun = kernel.as[ScalaFunction `(` Input `)=>` Array[Float]]
        kernelFun(input `;`)
        val (kernelOutput, time) = kernelFun(input `;`)
        println(s"; runtime is $time")
        val correct = Correctness(kernelOutput, scalaOutput)
        if (verbose) {
          println(s"For program:${dpiaProgram.name}")
          println(s"DPIA code:$dpiaSource")
          println(s"Generated code:${kernel.code}")
        }
        (time.value, makeOutput(dpiaProgram.name, conf.paramMap, conf.inputSize, conf.localSize, conf.globalSize, kernel.code, time, correct))
      }
      val median = tenRuns.sortBy(_._1).apply(runsPerProgram/2)._2
      resultPrintout(median).foreach(string => println(s"Median result: $string"))
      median
    }

    opencl.executor.Executor.shutdown()
    results
  }
}