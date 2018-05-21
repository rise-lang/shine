
import dot.high_level
import idealised.C
import idealised.OpenMP
import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.OpenCL._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._
import lift.arithmetic._
import opencl.executor.Executor

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.util.Random

object asum extends App {

//  Executor.loadLibrary()
//  Executor.init()

  val check = true
  val N = SizeVar("N")
  val inputT = ArrayType(N, float)

  def runOpenCLKernel(name: String, expr: Expr[DataType -> DataType]): Unit = {

    println(s"-- $name --")
    val phrase = TypeInference(expr, Map()).convertToPhrase
    val kernel = KernelGenerator.makeKernel(phrase, localSize = 128, globalSize = N)
    println(kernel.code)

//    val fun = kernel.as[ScalaFunction `(` Array[Float] `)=>` Array[Float]]
//
//    val size = 1024 * 1024
//
//    val input = Array.fill(size)(Random.nextInt(10).toFloat)
//
//    val (res, time) = fun(input `;`)
//    println(s"RESULT NAME: $name TIME: $time")
//    if (check) {
//      val gold = input.map(math.abs).sum
//      if (res.sum == gold) {
//        println(s"Computed result MATCHES with gold solution.")
//      } else {
//        println(s"ERROR computed result (${res.sum}) differs from gold ($gold) solution.")
//      }
//    }
//
    println("----------------\n")
  }

  //val abs = (t: DataType) => fun(x => oclFun("fabs", t, t, x))
  val abs = (t: DataType) => fun(x => foreignFun(t, "abs", (t, "y"), "{ return fabs(y); }", x))
  val add = fun(x => fun(a => x + a))

  val high_level = fun(inputT)(input =>
    reduce(add, 0.0f) o map(abs(float)) $ input)

  {
    println(s"-- high level --")
    val phrase = TypeInference(high_level, Map()).convertToPhrase
//    val program = C.ProgramGenerator.makeCode(phrase)
    val program = OpenMP.ProgramGenerator.makeCode(phrase)
    println(program.code)
  }

  System.exit(1)

  {
    println(s"-- high level --")
    val phrase = TypeInference(high_level, Map()).convertToPhrase
    val kernel = KernelGenerator.makeKernel(phrase, localSize = 128, globalSize = N)
    println(kernel.code)
  }

  val intelDerivedNoWarp1 = fun(inputT)(input =>
    join() o mapWorkgroup(
      asScalar() o mapLocal(
        reduceSeq(fun(x => fun(a => abs(float4)(x) + a)), vectorize(4, 0.0f))
      ) o split(8192) o asVector(4)
    ) o split(32768) $ input
  )
  runOpenCLKernel("intelDerivedNoWarp1", intelDerivedNoWarp1)

  val intelDerived2 = fun(inputT)(input =>
    join() o mapWorkgroup(
      mapLocal(
        reduceSeq(add, 0.0f)
      ) o split(2048)
    ) o split(2048) $ input
  )
  runOpenCLKernel("intelDerived2", intelDerived2)

  println(reorderWithStridePhrase.apply(128))

  val nvidiaDerived1 = fun(inputT)(input =>
    join() o mapWorkgroup(
      mapLocal(
        reduceSeq(fun(x => fun(a => abs(float)(x) + a)), 0.0f)
      ) o split(2048) o gather(reorderWithStridePhrase(128))
    ) o split(2048 * 128) $ input
  )
  runOpenCLKernel("nvidiaDerived1", nvidiaDerived1)

  val amdNvidiaDerived2 = fun(inputT)(input =>
    join() o mapWorkgroup(
      toLocal(iterate(6,
        mapLocal(
          reduceSeq(add, 0.0f)
        ) o split(2)
      )) o toLocal(mapLocal(
        reduceSeq(add, 0.0f)
      )) o split(128)
    ) o split(8192) $  input
  )
  runOpenCLKernel("amdNvidiaDerived2", amdNvidiaDerived2)

  val amdDerived1 = fun(inputT)(input =>
    join() o mapWorkgroup(
      asScalar() o mapLocal(
        reduceSeq(fun(x => fun(a => abs(float2)(x) + a)), vectorize(2, 0.0f))
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

//  Executor.shutdown()
}
