
import idealised.Core._
import idealised.DSL.untyped._
import idealised.OpenCL.Core._
import idealised.OpenCL.DSL._
import lift.arithmetic._
import idealised.Core.Time.ms
import opencl.executor.Executor
import opencl.generator.OpenCLPrinter

import scala.util.Random

object scal extends App {

  Executor.loadLibrary()
  Executor.init(1, 0)
  println("Platform: " + Executor.getPlatformName)
  println("Device: " + Executor.getDeviceName)

  val benchmark = true
  val iterations = 10

  val N = SizeVar("N")
  val dataT = float
  val inputT = ExpType(ArrayType(N, dataT))

  def runOpenCLKernel(name: String,
                      untypedLambda: Phrase[ExpType ->(ExpType -> ExpType)]) = {
    println("\n----------------")
    val lambda = TypeInference(untypedLambda)
    println(name + ":\n" + PrettyPhrasePrinter(lambda))
    lambda.typeCheck()

    println(s"-- $name --")
    val toOpenCL = ToOpenCL(localSize = 128, globalSize = N)
    val kernel = toOpenCL.makeKernel(lambda)
    println(OpenCLPrinter()(kernel))

    val fun = toOpenCL.asFunction[(Array[Float] :: Float :: Nil) =:=> Array[Float]](kernel)

    val size = 1024 * 1024 * 16

    val input = Array.fill(size)(Random.nextInt(4).toFloat)
    val alpha = Random.nextInt(4).toFloat

    if (benchmark) {
      var times = Vector[TimeSpan[ms]]()
      for (i <- 0 to iterations) {
        val (_, time) = fun(input :: alpha :: HNil)
        times = times :+ time
      }
      val sorted = times.sortBy(_.value)
      println(s"RESULT NAME: $name MEDIAN: ${sorted(sorted.length/2)} MIN: ${sorted.head} MAX: ${sorted.last}")
    } else {
      val (res, time) = fun(input :: alpha :: HNil)
      println(s"RESULT NAME: $name TIME: $time")
      val gold = input.sum * alpha
      if (res.sum == gold) {
        println(s"Computed result MATCHES with gold solution.")
      } else {
        println(s"ERROR computed result differs from gold solution.")
      }
    }
    println("----------------\n")
  }

  val high_level = λ(inputT)(input => λ(ExpType(dataT))(alpha =>
    map(λ( x => alpha * x ), input)
  ) )

  {
    val lambda = TypeInference(high_level)
    println("high_level:\n" + PrettyPhrasePrinter(lambda))
    lambda.typeCheck()
  }

  val scalWgLcl = (fst: ArithExpr, snd: ArithExpr) =>
    λ(inputT)(input => λ(ExpType(dataT))(alpha =>
      join() o mapWorkgroup(
        join() o mapLocal(mapSeq(
          λ(x => alpha * x)
        )) o split(snd)
      ) o split(fst) $ input
    ))

  runOpenCLKernel("vectorScal", scalWgLcl(1024, 4))

  runOpenCLKernel("scalAMD", scalWgLcl(128, 1))

  runOpenCLKernel("scalNvidia", scalWgLcl(2048, 1))

  val scalIntel = λ(inputT)(input => λ(ExpType(dataT))(alpha =>
    join() o mapWorkgroup(
      asScalar() o join() o mapLocal(mapSeq(
        λ(x => vectorize(4, alpha) * x)
      )) o split(128) o asVector(4)
    ) o split(4 * 128 * 128) $ input
  ))

  runOpenCLKernel("scalIntel", scalIntel)

  Executor.shutdown()
}
