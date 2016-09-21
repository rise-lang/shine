
import idealised._
import idealised.Core._
import idealised.DSL.untyped._
import idealised.OpenCL.Core._
import idealised.OpenCL.DSL._
import apart.arithmetic._
import opencl.executor.Executor
import opencl.generator.OpenCLPrinter

import scala.language.implicitConversions

object asum extends App {

  Executor.loadLibrary()
  Executor.init()

  val N = SizeVar("N")
  val inputT = ExpType(ArrayType(N, float))

  def runOpenCLKernel(name: String,
                      untypedLambda: Phrase[ExpType -> ExpType]) = {
    println("\n----------------")
    val lambda = TypeInference(untypedLambda)
    println(name + ":\n" + PrettyPrinter(lambda))
    lambda.typeCheck()

    println(s"-- $name --")
    val toOpenCL = ToOpenCL(localSize = 128, globalSize = N)
    val kernel = toOpenCL.makeKernel(lambda)
    println(OpenCLPrinter()(kernel))

    val fun = toOpenCL.asFunction[(Array[Float] :: Nil) =:=> Array[Float]](kernel)

    val size = 1024 * 1024

    val input = Array.fill(size)(1.0f)

    val (res, time) = fun(input :: HNil)

    println(s"Computed ${res.length} partial results in $time, which add up to ${res.sum} (expected ${input.sum}).")
    print("[")
    res.foreach(x => print(s"$x "))
    println("]")

    println("----------------\n")
  }

  val abs = λ(x => `if`(x < 0.0f, -x, x))
  val add = λ(x => λ(a => x + a))

  val high_level = λ(inputT)(input =>
    reduce(add, 0.0f) o map(abs) $ input
  )

  {
    val lambda = TypeInference(high_level)
    println("high_level:\n" + PrettyPrinter(lambda))
    lambda.typeCheck()
  }

  val intelDerivedNoWarp1 = λ(inputT)(input =>
    mapWorkgroup(
      /*asScalar() o */ mapLocal(
        reduceSeq(λ(x => λ(a => abs(x) + a)), /* asVector(4) */ 0.0f)
      ) o split(8192) /* o asVector(4) */
    ) o split(32768) $ input
  )
  runOpenCLKernel("intelDerivedNoWarp1", intelDerivedNoWarp1)

  val intelDerived2 = λ(inputT)(input =>
    mapWorkgroup(
      mapLocal(
        reduceSeq(add, 0.0f)
      ) o split(2048)
    ) o split(2048) $ input
  )
  runOpenCLKernel("intelDerived2", intelDerived2)

  val nvidiaDerived1 = λ(inputT)(input =>
    mapWorkgroup(
      mapLocal(
        reduceSeq(add, 0.0f)
      ) o split(2048) o gather(reorderWithStridePhrase(128))
    ) o split(2048 * 128) $ input
  )
  runOpenCLKernel("nvidiaDerived1", nvidiaDerived1)

  val nvidiaDerived2 = λ(inputT)(input =>
    mapWorkgroup(
      toLocal(iterate(6,
        mapLocal(reduceSeq(add, 0.0f)) o
          split(2)
      ))
        o
        toLocal(mapLocal(
          reduceSeq(add, 0.0f)
        )) o split(128)
    ) o split(8192) $ input
  )
  runOpenCLKernel("nvidiaDerived2", nvidiaDerived2)

  val amdDerived1 = λ(inputT)(input =>
    mapWorkgroup(
      asScalar() o mapLocal(
        reduceSeq(λ(x => λ(a => x + a)), vectorize(2, 0.0f))
      ) o split(2048) o gather(reorderWithStridePhrase(64)) o asVector(2)
    ) o split(4096 * 128) $ input
  )
  runOpenCLKernel("amdDerived1", amdDerived1)

  Executor.shutdown()
}
