package benchmarks.core

import benchmarks.core.OpenCLBenchmark.{Configuration, DpiaProgram, Parameter}
import idealised.OpenCL.Kernel
import idealised.SurfaceLanguage.{->, Expr}
import idealised.SurfaceLanguage.Types.{DataType, Type, TypeInference}
import idealised.utils.{Time, TimeSpan}
import lift.arithmetic.ArithExpr

import scala.util.Random


object OpenCLBenchmark {
  case class Parameter(name:String, values:Seq[Int])

  def makeSpace(params:List[Parameter]):Seq[Map[String,Int]] = {
    params match {
      case Nil => Seq(Map())
      case x::xs =>
        makeSpace(xs).flatMap(valueMap => x.values.map(value => valueMap + (x.name -> value)))
    }
  }

  case class Configuration(inputSize:Int, localSize:Int, globalSize:Int, paramMap:Map[String,Int]) {
    def printOut(): Unit = {
      println(s"{ inputSize = $inputSize, localSize = $localSize, globalSize = $globalSize, paramMap = $paramMap })")
    }
  }

  case class DpiaProgram(name:String, makeProgram:Configuration => Expr[DataType -> DataType])
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

  private def compile(dpiaProgram:Expr[DataType -> DataType], configuration: Configuration):Kernel = {
    idealised.OpenCL.KernelGenerator.makeCode(TypeInference(dpiaProgram, Map()).toPhrase, configuration.localSize, configuration.globalSize)
  }

  final def explore(inputSize:Parameter, localSizeParam:Parameter, customParams:Set[Parameter], checkCorrectness:Boolean):Seq[Result] = {
    OpenCLBenchmark.makeSpace(customParams.toList).flatMap(run(inputSize, localSizeParam, _, checkCorrectness))
  }



  final def run(inputSizeParam:Parameter,
                localSizeParam:Parameter,
                paramMap:Map[String, Int],
                checkCorrectness:Boolean):Seq[Result] = {
    inputSizeParam.values.flatMap(inputSize => {
      println(s"For input size:$inputSize")
      val rand = new Random()
      val input = makeInput(inputSize, rand)
      val scalaOutput = if(checkCorrectness) Some(runScalaProgram(input, paramMap)) else None
      val globalSize = inputSize
      localSizeParam.values.flatMap(localSize => {
        val configuration = Configuration(inputSize, localSize, globalSize, paramMap)
        println("Running configuration")
        configuration.printOut()
        runWithConfiguration(input, scalaOutput, configuration)
      })
    })
  }

  private def runWithConfiguration(input:Input,
                                   scalaOutput:Option[Array[Float]],
                                   conf: Configuration):Seq[Result] = {


    opencl.executor.Executor.loadAndInit()

    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}

    val results = for(dpiaProgram <- this.dpiaPrograms) yield {
      print(s"Running '${dpiaProgram.name}'")
      val dpiaSource = dpiaProgram.makeProgram(conf)
      val kernel = this.compile(dpiaSource, conf)
      val kernelFun = kernel.as[ScalaFunction `(` Input `)=>` Array[Float]]

      val tenRuns = for(runNum <- 0 until runsPerProgram) yield {
        print(s"$runNum:'")
        kernelFun(input `;`)
        val (kernelOutput, time) = kernelFun(input `;`)
        println(s"runtime is $time")
        val correct = scalaOutput.map(Correctness(kernelOutput, _)).getOrElse(Unchecked)
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