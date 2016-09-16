
import idealised.Core._
import idealised.DSL.untyped._
import idealised.OpenCL.Core._
import idealised.OpenCL.DSL._
import apart.arithmetic._
import opencl.executor.Executor
import opencl.generator.OpenCLPrinter

import scala.language.implicitConversions

object gemv extends App {

  Executor.loadLibrary()
  Executor.init()

  val N = SizeVar("N")
  val M = SizeVar("M")
  val dataT = float
  val xsT = ExpType(ArrayType(N, dataT))
  val ysT = ExpType(ArrayType(M, dataT))
  val matT = ExpType(ArrayType(M, ArrayType(N, dataT)))

  def runOpenCLKernel(name: String,
                      untypedLambda: Phrase[ExpType -> (ExpType -> (ExpType -> (ExpType -> (ExpType -> ExpType))))]) = {
    println("\n----------------")
    val lambda = TypeInference(untypedLambda)
    println(name + ":\n" + PrettyPrinter(lambda))
    lambda.typeCheck()

    println(s"-- $name --")
    val toOpenCL = ToOpenCL(localSize = 32, globalSize = M * 32)
    val kernel = toOpenCL.makeKernel(lambda)
    println(OpenCLPrinter()(kernel))

    val fun = toOpenCL.asFunction[(Array[Array[Float]] :: Array[Float] :: Array[Float] :: Float :: Float :: Nil) =:=> Array[Float]](kernel)

    val size = 512

    val mat = Array.tabulate(size, size)((_, _) => 1.0f)
    val xs = Array.fill(size)(1.0f)
    val ys = Array.fill(size)(2.0f)

    val (res, time) = fun(mat :: xs :: ys :: 4.5f :: 5.6f)

    println(time)

    println("----------------\n")
  }

  val mult = λ(x => x._1 * x._2)
  val add = λ(x => λ(a => x + a))
  val scal = λ(xs => λ(alpha => map(λ(x => alpha * x), xs)))
  val dot = λ(xs => λ(ys => reduce(add, 0.0f) o map(mult) $ zip(xs, ys)))

  val high_level =
    λ(matT)(mat => λ(xsT)(xs => λ(ysT)(ys =>
      λ(ExpType(dataT))(alpha => λ(ExpType(dataT))(beta =>

        map(λ(x => x._1 + x._2)) $
          zip(map(λ(row => alpha * dot(row)(xs)), mat), scal(ys)(beta))

      )))))

  {
    val lambda = TypeInference(high_level)
    println("high_level:\n" + PrettyPrinter(lambda))
    lambda.typeCheck()
  }

  val fullMatrixVectorFusedOpenCL =
    λ(matT)(mat => λ(xsT)(xs => λ(ysT)(ys =>
      λ(ExpType(dataT))(alpha => λ(ExpType(dataT))(beta =>

        join() o mapWorkgroup(λ(t =>
          mapLocal(λ(x => (alpha * x) + (t._2 * beta))) o
            toLocal(mapLocal(reduceSeq(λ(x => λ(a => mult(x) + a)), 0.0f)))
            o split(N) $ zip(xs, t._1)
        )) $ zip(mat, ys)

      )))))

  runOpenCLKernel("fullMatrixVectorFusedOpenCL", fullMatrixVectorFusedOpenCL)

  val fullMatrixVectorFusedOpenCLAMD =
    λ(matT)(mat => λ(xsT)(xs => λ(ysT)(ys =>
      λ(ExpType(dataT))(alpha => λ(ExpType(dataT))(beta =>

        join() o mapWorkgroup(λ(t =>
          mapLocal(λ(x => (alpha * x) + (t._2 * beta))) o
            mapLocal(reduceSeq(add, 0.0f)) o split(128) o
            toLocal(mapLocal(reduceSeq(λ(x => λ(a => mult(x) + a)), 0.0f)))
            o split(N /^ 128) o gather(reorderWithStridePhrase(128)) $ zip(xs, t._1)
        )) $ zip(mat, ys)

      )))))

  runOpenCLKernel("fullMatrixVectorFusedOpenCLAMD", fullMatrixVectorFusedOpenCLAMD)

  val keplerBest =
    λ(matT)(mat => λ(xsT)(xs => λ(ysT)(ys =>
      λ(ExpType(dataT))(alpha => λ(ExpType(dataT))(beta =>

        mapWorkgroup(λ(t =>
          λ(x => (x * alpha) + (t._2 * beta)) o
            toLocal(reduceSeq(add, 0.0f)) o
            toLocal(mapLocal(reduceSeq(λ(x => λ(a => mult(x) + a)), 0.0f))) o
            split(N /^ 128) o gather(reorderWithStridePhrase(128)) $ zip(xs, t._1)
        )) $ zip(mat, ys)

      )))))

  runOpenCLKernel("keplerBest", keplerBest)

  Executor.shutdown()

}
