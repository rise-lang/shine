
import idealised.DPIA.Phrases.PrettyPhrasePrinter
import idealised.DPIA.Types.TypeCheck
import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.OpenCL._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, _}
import idealised.utils.{Time, TimeSpan}
import lift.arithmetic._

import scala.util.Random

object scal extends App {

//  Executor.loadLibrary()
//  Executor.init(0, 0)
//  println("Platform: " + Executor.getPlatformName)
//  println("Device: " + Executor.getDeviceName)

  val benchmark = true
  val iterations = 10

  val N = SizeVar("N")
  val inputT = ArrayType(N, float)

  def runOpenCLKernel(name: String,
                      untypedLambda: Expr[DataType -> (DataType -> DataType)]): Unit = {
    println("\n----------------")
    val lambda = TypeInference(untypedLambda, Map()).convertToPhrase
    println(name + ":\n" + PrettyPhrasePrinter(lambda))
    TypeCheck(lambda)

    println(s"-- $name --")
    val kernel = KernelGenerator.makeCode(localSize = 128, globalSize = N)(lambda)
    println(kernel.code)

    val fun = kernel.as[ScalaFunction `(` Array[Float] `,` Float `)=>` Array[Float]]

    val size = 1024 * 1024 * 16

    val input = Array.fill(size)(Random.nextInt(4).toFloat)
    val alpha = Random.nextInt(4).toFloat

    if (benchmark) {
      var times = Vector[TimeSpan[Time.ms]]()
      for (_ <- 0 to iterations) {
        val (_, time) = fun(input `,` alpha)
        times = times :+ time
      }
      val sorted = times.sortBy(_.value)
      println(s"RESULT NAME: $name MEDIAN: ${sorted(sorted.length/2)} MIN: ${sorted.head} MAX: ${sorted.last}")
    } else {
      val (res, time) = fun(input `,` alpha)
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

  val high_level = fun(inputT)(input => fun(float)(alpha =>
    mapSeq(fun(x => alpha * x ), input)
  ) )

  {
    val lambda = TypeInference(high_level, Map()).convertToPhrase
    println("high_level:\n" + PrettyPhrasePrinter(lambda))
    TypeCheck(lambda)
  }

  val scalWgLcl = (fst: ArithExpr, snd: ArithExpr) =>
    fun(inputT)(input => fun(float)(alpha =>
      join() o mapWorkgroup(
        join() o mapLocal(mapSeq(
          fun(x => alpha * x)
        )) o split(snd)
      ) o split(fst) $ input
    ))

//  runOpenCLKernel("vectorScal", scalWgLcl(1024, 4))
//
//  runOpenCLKernel("scalAMD", scalWgLcl(128, 1))
//
//  runOpenCLKernel("scalNvidia", scalWgLcl(2048, 1))

  val scalIntel = fun(inputT)(input => fun(float)(alpha =>
    join() o mapWorkgroup(
      asScalar() o join() o mapLocal(mapSeq(
        fun(x => vectorize(4, alpha) * x)
      )) o split(128) o asVector(4)
    ) o split(4 * 128 * 128) $ input
  ))

//  runOpenCLKernel("scalIntel", scalIntel)

  {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val scalIntel = fun(inputT)(input => fun(float)(alpha =>
      join() o mapPar(
        asScalar() o join() o mapSeq(mapSeq(
          fun(x => vectorize(4, alpha) * x)
        )) o split(128) o asVector(4)
      ) o split(4 * 128 * 128) $ input
    ))

    val phrase = TypeInference(scalIntel, Map()).toPhrase
    val program = idealised.OpenMP.ProgramGenerator.makeCode(phrase, "scalIntel")
    println(program.code)
  }

  {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val scalIntel2 = fun(inputT)(input => fun(float)(alpha =>
      join() o mapPar(
        asScalar() o mapSeq(
          fun(x => vectorize(4, alpha) * x)
        ) o asVector(4)
      ) o split(4 * 128 * 128) $ input
    ))

    val phrase = TypeInference(scalIntel2, Map()).toPhrase
    val program = idealised.OpenMP.ProgramGenerator.makeCode(phrase, "scalIntel2")
    println(program.code)
  }

//  Executor.shutdown()
}
