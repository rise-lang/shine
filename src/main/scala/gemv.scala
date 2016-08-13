
import idealised.Core._
import idealised.DSL.untyped._
import idealised.OpenCL.Core._
import idealised.OpenCL.DSL._
import apart.arithmetic._
import opencl.generator.OpenCLPrinter

import scala.language.implicitConversions

object gemv extends App {

  val N = SizeVar("N")
  val M = SizeVar("M")
  val dataT = float
  val xsT = ExpType(ArrayType(N, dataT))
  val ysT = ExpType(ArrayType(M, dataT))
  val matT = ExpType(ArrayType(M, ArrayType(N, dataT)))

  def printOpenCLKernel1(name: String,
                         untypedLambda: Phrase[ExpType -> (ExpType -> (ExpType -> (ExpType -> (ExpType -> ExpType))))]) = {
    val lambda = TypeInference(untypedLambda)
    println(name + ":\n" + PrettyPrinter(lambda))
    lambda.typeCheck()

    println(s"-- $name --")
    val toOpenCL = ToOpenCL(localSize = 128, globalSize = M * 128)
    val kernel = toOpenCL.makeKernel(lambda)
    println(OpenCLPrinter()(kernel))

    val fun = toOpenCL.asFunction[(Array[Array[Float]]::Array[Float]::Array[Float]::Float::Float::Nil) =:=> Array[Float]](kernel)

    val mat = Array.tabulate(32, 32)((_, _) => 1.0f)
    val xs = Array.fill(32)(1.0f)
    val ys = Array.fill(32)(2.0f)

    val (res, time) = fun(mat::xs::ys::4.5f::5.6f)

    println(time)

    println("----------------")
  }

  val mult = λ( x => x._1 * x._2 )
  val add = λ( x => λ( a => x + a))
  val scal = λ(xs => λ(alpha => map(λ( x => alpha * x ), xs) ) )
  val dot = λ(xs => λ(ys => reduce(add, 0.0f) o map(mult) $ zip(xs, ys) ) )

  val high_level =
    λ(matT)(mat => λ(xsT)(xs => λ(ysT)(ys =>
      λ(ExpType(dataT))(alpha => λ(ExpType(dataT))(beta =>

        map(λ( x => x._1 + x._2 )) $
        zip(map(λ(row => alpha * dot(row)(xs)), mat), scal(ys)(beta))

    ) ) ) ) )

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

      ) ) ) ) )

  printOpenCLKernel1("fullMatrixVectorFusedOpenCL", fullMatrixVectorFusedOpenCL)

  val fullMatrixVectorFusedOpenCLAMD =
    λ(matT)(mat => λ(xsT)(xs => λ(ysT)(ys =>
      λ(ExpType(dataT))(alpha => λ(ExpType(dataT))(beta =>

        join() o mapWorkgroup(λ(t =>
          mapLocal(λ(x => (alpha * x) + (t._2 * beta))) o
            mapLocal(reduceSeq(add, 0.0f)) o split(128) o
            toLocal(mapLocal(reduceSeq(λ(x => λ(a => mult(x) + a)), 0.0f)))
            o split(N /^ 128) o gather(reorderWithStridePhrase(128)) $ zip(xs, t._1)
        )) $ zip(mat, ys)

      ) ) ) ) )

  printOpenCLKernel1("fullMatrixVectorFusedOpenCLAMD", fullMatrixVectorFusedOpenCLAMD)

  val keplerBest =
    λ(matT)(mat => λ(xsT)(xs => λ(ysT)(ys =>
      λ(ExpType(dataT))(alpha => λ(ExpType(dataT))(beta =>

        mapWorkgroup(λ(t =>
          λ(x => (x * alpha) + (t._2 * beta)) o
            toLocal(reduceSeq(add, 0.0f)) o
            toLocal(mapLocal(reduceSeq(λ(x => λ(a => mult(x) + a)), 0.0f))) o
            split(N /^ 128) o gather(reorderWithStridePhrase(128)) $ zip(xs, t._1)
        )) $ zip(mat, ys)

      ) ) ) ) )

  printOpenCLKernel1("keplerBest", keplerBest)

}
