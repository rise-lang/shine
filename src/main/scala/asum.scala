
import idealised.Core._
import idealised.DSL.untyped._
import idealised.OpenCL.Core._
import idealised.OpenCL.DSL._
import lift.arithmetic._
import idealised.OpenCL.LowLevelPrimitives.UnaryOpenCLFunction
import opencl.executor.Executor
import opencl.generator.OpenCLPrinter

import scala.language.implicitConversions
import scala.util.Random

object asum extends App {

  Executor.loadLibrary()
  Executor.init()

  val check = true
  val N = SizeVar("N")
  val inputT = ExpType(ArrayType(N, float))

  def runOpenCLKernel(name: String,
                      untypedLambda: Phrase[ExpType -> ExpType]) = {
    println("\n----------------")
    val lambda = TypeInference(untypedLambda)
    println(name + ":\n" + PrettyPhrasePrinter(lambda))
    lambda.typeCheck()

    println(s"-- $name --")
    val toOpenCL = ToOpenCL(localSize = 128, globalSize = N)
    val kernel = toOpenCL.makeKernel(lambda)
    println(OpenCLPrinter()(kernel))

    val fun = toOpenCL.asFunction[(Array[Float] :: Nil) =:=> Array[Float]](kernel)

    val size = 1024 * 1024

    val input = Array.fill(size)(Random.nextInt(10).toFloat)

    val (res, time) = fun(input :: HNil)
    println(s"RESULT NAME: $name TIME: $time")
    if (check) {
      val gold = input.map(math.abs).sum
      if (res.sum == gold) {
        println(s"Computed result MATCHES with gold solution.")
      } else {
        println(s"ERROR computed result differs from gold solution.")
      }
    }

    println("----------------\n")
  }

  //val abs = λ(x => `if`(x < 0.0f, -x, x))
  def abs(t: DataType) = λ(x => UnaryOpenCLFunction("fabs", t, t, x) )
  val add = λ(x => λ(a => x + a))

  val high_level = λ(inputT)(input =>
    reduce(add, 0.0f) o map(abs(float)) $ input
  )

  {
    val lambda = TypeInference(high_level)
    println("high_level:\n" + PrettyPhrasePrinter(lambda))
    lambda.typeCheck()
  }

  val intelDerivedNoWarp1 = λ(inputT)(input =>
    join() o mapWorkgroup(
      asScalar() o mapLocal(
        reduceSeq(λ(x => λ(a => abs(float4)(x) + a)), vectorize(4, 0.0f))
      ) o split(8192) o asVector(4)
    ) o split(32768) $ input
  )
  runOpenCLKernel("intelDerivedNoWarp1", intelDerivedNoWarp1)

  val intelDerived2 = λ(inputT)(input =>
    join() o mapWorkgroup(
      mapLocal(
        reduceSeq(add, 0.0f)
      ) o split(2048)
    ) o split(2048) $ input
  )
  runOpenCLKernel("intelDerived2", intelDerived2)

  val nvidiaDerived1 = λ(inputT)(input =>
    join() o mapWorkgroup(
      mapLocal(
        reduceSeq(λ(x => λ(a => abs(float)(x) + a)), 0.0f)
      ) o split(2048) o gather(reorderWithStridePhrase(128))
    ) o split(2048 * 128) $ input
  )
  runOpenCLKernel("nvidiaDerived1", nvidiaDerived1)

  val amdNvidiaDerived2 = λ(inputT)(input =>
    join() o mapWorkgroup(
      toLocal(iterate(6,
        mapLocal(reduceSeq(add, 0.0f)) o split(2)
      )) o toLocal(mapLocal(
          reduceSeq(add, 0.0f)
        )) o split(128)
    ) o split(8192) $ input
  )
  runOpenCLKernel("amdNvidiaDerived2", amdNvidiaDerived2)

  val amdDerived1 = λ(inputT)(input =>
    join() o mapWorkgroup(
      asScalar() o mapLocal(
        reduceSeq(λ(x => λ(a => abs(float2)(x) + a)), vectorize(2, 0.0f))
      ) o split(2048) o gather(reorderWithStridePhrase(64)) o asVector(2)
    ) o split(4096 * 128) $ input
  )
//  val amdDerived1 = λ(inputT)(input =>
//    mapWorkgroup(
//      join() o mapLocal(
//        mapSeq(reduceSeq(λ(x => λ(a => x + a)), 0.0f))
//      ) o split(2) o split(2048) /*o gather(reorderWithStridePhrase(64)) */
//    ) o split(4096 * 128) $ input
//  )
  runOpenCLKernel("amdDerived1", amdDerived1)

  Executor.shutdown()
}
