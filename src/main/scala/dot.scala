
import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.OpenCL._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._
import lift.arithmetic._
import opencl.executor.Executor
import idealised._

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.util.Random

object dot extends App {

//  Executor.loadLibrary()
//  Executor.init()

  val epsilon = 1.0f

  val check = true
  val N = SizeVar("N")
  val xsT = ArrayType(N, float)
  val ysT = ArrayType(N, float)

  def printOpenCLKernel1(name: String, expr: Expr[DataType -> (DataType -> DataType)]): Unit = {
    println(s"-- $name --")

    val kernel = KernelGenerator.makeCode(TypeInference(expr, Map()).toPhrase, localSize = 128, globalSize = N)
    println(kernel.code)

    val fun = kernel.as[ScalaFunction `(` Array[Float] `,` Array[Float] `)=>` Array[Float]]

    val size = 1024 * 1024

    val xs = Array.fill(size)(Random.nextInt(4).toFloat)
    val ys = Array.fill(size)(Random.nextInt(4).toFloat)

    val (res, time) = fun(xs `,` ys)
    println(s"RESULT KERNEL1 NAME: $name TIME: $time")
    if (check) {
      val gold = (xs zip ys).map{ case (x, y) => x * y }.sum
      if (res.sum - gold < epsilon) {
        println(s"Computed result MATCHES with gold solution.")
      } else {
        println(s"ERROR computed result differs from gold solution.")
        println(s"res: ${res.sum} vs. gold: $gold")
      }
    }

    println("----------------\n")
  }

  def printOpenCLKernel2(name: String, expr: Expr[DataType -> DataType]): Unit = {
    println(s"-- $name --")
    val kernel = KernelGenerator.makeCode(TypeInference(expr, Map()).toPhrase, localSize = 128, globalSize = N)
    println(kernel.code)

    val fun = kernel.as[ScalaFunction `(` Array[Float] `)=>` Array[Float]]

    val size = 512

    val xs = Array.fill(size)(Random.nextInt(10).toFloat)

    val (res, time) = fun(xs `;`)
    println(s"RESULT KERNEL2 NAME: $name TIME: $time")
    if (check) {
      val gold = xs.sum
      if (res.sum - gold < epsilon) {
        println(s"Computed result MATCHES with gold solution.")
      } else {
        println(s"ERROR computed result differs from gold solution.")
      }
    }

    println("----------------\n")
  }

  val mult = fun(x => x._1 * x._2)
  val add = fun((x, a) => x + a)

  val high_level = fun(xsT)(xs => fun(ysT)(ys =>
    reduce(add, 0.0f) o mapSeq(mult) $ zip(xs, ys)
  ))

//  {
//    println(s"-- high level --")
//    val phrase = TypeInference(high_level, Map()).convertToPhrase
//    val program = C.ProgramGenerator.makeCode(phrase)
//    println(program.code)
//  }

  // OpenMP specific stuff

  {
    println(s"-- high level --")
    val phrase = TypeInference(high_level, Map()).convertToPhrase
    val program = OpenMP.ProgramGenerator.makeCode(phrase)
    println(program.code)
  }

  {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val dotCPUVector1 = fun(xsT)(xs => fun(ysT)(ys =>
      asScalar() o join() o mapPar(
        mapSeq(
          reduceSeq(fun(x => fun(a => mult(x) + a)), vectorize(4, 0.0f))
        ) o split(2048)
      ) o split(2048 * 64) $ zip(asVector(4) $ xs, asVector(4) $ ys)
    ))
    val phrase = TypeInference(dotCPUVector1, Map()).toPhrase
    val program = OpenMP.ProgramGenerator.makeCode(phrase)
    println(program.code)
  }

  {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val intelDerivedNoWarpDot1 = fun(xsT)(xs => fun(ysT)(ys =>
      asScalar() o join() o mapPar(
        mapSeq(
          reduceSeq(fun(x => fun(a => mult(x) + a)), vectorize(4, 0.0f))
        ) o split(8192)
      ) o split(8192) $ zip(asVector(4) $ xs, asVector(4) $ ys)
    ))
    val phrase = TypeInference(intelDerivedNoWarpDot1, Map()).toPhrase
    val program = OpenMP.ProgramGenerator.makeCode(phrase)
    println(program.code)
  }

  {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val dotCPU1 = fun(xsT)(xs => fun(ysT)(ys =>
      join() o mapPar(
        mapSeq(
          reduceSeq(fun(x => fun(a => mult(x) + a)), 0.0f)
        ) o split(2048)
      ) o split(2048 * 128) $ zip(xs, ys)
    ))
    val phrase = TypeInference(dotCPU1, Map()).toPhrase
    val program = OpenMP.ProgramGenerator.makeCode(phrase)
    println(program.code)
  }

  {
    import idealised.OpenMP.SurfaceLanguage.DSL._
    val dotCPU2 = fun(xsT)(in =>
      join() o mapPar(
        mapSeq(
          reduceSeq(fun(x => fun(a => x + a)), 0.0f)
        ) o split(128)
      ) o split(128) $ in
    )
    val phrase = TypeInference(dotCPU2, Map()).toPhrase
    val program = OpenMP.ProgramGenerator.makeCode(phrase)
    println(program.code)
  }

//  sys.exit(1)

  // OpenCL specific stuff

  val dotCPUVector1 = fun(xsT)(xs => fun(ysT)(ys =>
    asScalar() o join() o mapWorkgroup(
      mapLocal(
        reduceSeq(fun(x => fun(a => mult(x) + a)), vectorize(4, 0.0f))
      ) o split(2048)
    ) o split(2048 * 64) $ zip(asVector(4) $ xs, asVector(4) $ ys)
  ))

  val intelDerivedNoWarpDot1 = fun(xsT)(xs => fun(ysT)(ys =>
    asScalar() o join() o mapWorkgroup(
      mapLocal(
        reduceSeq(fun(x => fun(a => mult(x) + a)), vectorize(4, 0.0f))
      ) o split(8192)
    ) o split(8192) $ zip(asVector(4) $ xs, asVector(4) $ ys)
  ))

  printOpenCLKernel1("intelDerivedNoWarpDot1", intelDerivedNoWarpDot1)

  val dotCPU1 = fun(xsT)(xs => fun(ysT)(ys =>
    join() o mapWorkgroup(
      mapLocal(
        reduceSeq(fun(x => fun(a => mult(x) + a)), 0.0f)
      ) o split(2048)
    ) o split(2048 * 128) $ zip(xs, ys)
  ))

  printOpenCLKernel1("dotCPU1", dotCPU1)

  val dotCPU2 = fun(xsT)(in =>
    join() o mapWorkgroup(
      mapLocal(
        reduceSeq(fun(x => fun(a => x + a)), 0.0f)
      ) o split(128)
    ) o split(128) $ in
  )

  printOpenCLKernel2("dotCPU2", dotCPU2)

  val dotProduct1 = fun(xsT)(xs => fun(ysT)(ys =>
    join() o mapWorkgroup(
      mapLocal(
        reduceSeq(fun(x => fun(a => mult(x) + a)), 0.0f)
      ) o split(2048) o gather(reorderWithStridePhrase(128))
    ) o split(2048 * 128) $ zip(xs, ys)
  ))

  printOpenCLKernel1("dotProduct1", dotProduct1)

  val dotProduct2 = fun(xsT)(in =>
    join() o mapWorkgroup(
      iterate(6, toLocal(mapLocal(reduceSeq(add, 0.0f))) o split(2)) o
        toLocal(mapLocal(reduceSeq(add, 0.0f))) o split(2)
    ) o split(128) $ in
  )

  printOpenCLKernel2("dotProduct2", dotProduct2)

}

