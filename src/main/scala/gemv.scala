
import idealised.Core._
import idealised.DSL.untyped._
import idealised.OpenCL.Core._
import idealised.OpenCL.DSL._
import lift.arithmetic._
import opencl.executor.Executor
import opencl.generator.OpenCLPrinter

import scala.language.implicitConversions
import scala.util.Random

object gemv extends App {

  Executor.loadLibrary()
  Executor.init()

  val check = false // the floating point comparisons are killing the comparison (it passes with integer values)
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
    println(name + ":\n" + PrettyPhrasePrinter(lambda))
    lambda.typeCheck()

    println(s"-- $name --")
    val toOpenCL = ToOpenCL(localSize = 32, globalSize = M * 32)
    val kernel = toOpenCL.makeKernel(lambda)
    println(OpenCLPrinter()(kernel))

    val fun = toOpenCL.asFunction[(Array[Array[Float]] :: Array[Float] :: Array[Float] :: Float :: Float :: Nil) =:=> Array[Float]](kernel)

    val size = 512

    val mat = Array.tabulate(size, size)((_, _) => Random.nextInt(2).toFloat)
    val xs = Array.fill(size)(Random.nextInt(2).toFloat)
    val ys = Array.fill(size)(Random.nextInt(2).toFloat)
    val alpha = Random.nextInt(2).toFloat
    val beta = Random.nextInt(2).toFloat

    val (res, time) = fun(mat :: xs :: ys :: alpha :: beta)
    println(s"RESULT NAME: $name TIME: $time")
    if (check) {
      def dot = (xs: Array[Float], ys: Array[Float]) => (xs zip ys).map{ case (x, y) => x * y }.sum
      val gold = (mat.map(dot(xs, _) * alpha) zip ys.map(_ * beta)).map{ case (x, y) => x + y }

      if ((res zip gold).forall{ case (x, y) => x == y }) {
        println(s"Computed result MATCHES with gold solution.")
      } else {
        println(s"ERROR computed result differs from gold solution.")
      }
    }
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
    println("high_level:\n" + PrettyPhrasePrinter(lambda))
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
            toLocal(mapLocal(reduceSeq(add, 0.0f))) o split(128) o
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
