
import idealised.DPIA.Phrases.PrettyPhrasePrinter
import idealised.DPIA.Types.TypeCheck
import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.OpenCL._
import idealised.OpenMP
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, _}
import lift.arithmetic._

import scala.language.implicitConversions
import scala.util.Random

object gemv extends App {

//  Executor.loadLibrary()
//  Executor.init()

  val check = false // the floating point comparisons are killing the comparison (it passes with integer values)
  val N = SizeVar("N")
  val M = SizeVar("M")
  val dataT = float
  val xsT = ArrayType(N, dataT)
  val ysT = ArrayType(M, dataT)
  val matT = ArrayType(M, ArrayType(N, dataT))

  def runOpenCLKernel(name: String,
                      untypedLambda: Expr[DataType -> (DataType -> (DataType -> (DataType -> (DataType -> DataType))))]): Unit = {
    println("\n----------------")
    val lambda = TypeInference(untypedLambda, Map()).convertToPhrase
    println(name + ":\n" + PrettyPhrasePrinter(lambda))
    TypeCheck(lambda)

    println(s"-- $name --")
    val kernel = KernelGenerator.makeCode(lambda, localSize = 32, globalSize =  M * 32)
    println(kernel.code)

    val fun = kernel.as[ScalaFunction `(` Array[Array[Float]] `,` Array[Float] `,` Array[Float] `,` Float `,` Float `)=>` Array[Float]]

    val size = 512

    val mat = Array.tabulate(size, size)((_, _) => Random.nextInt(2).toFloat)
    val xs = Array.fill(size)(Random.nextInt(2).toFloat)
    val ys = Array.fill(size)(Random.nextInt(2).toFloat)
    val alpha = Random.nextInt(2).toFloat
    val beta = Random.nextInt(2).toFloat

    val args = mat `,` xs `,` ys `,` alpha `,` beta
    val (res, time) = fun(args)
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

  val mult = fun(x => x._1 * x._2)
  val add = fun(x => fun(a => x + a))
  val scal = fun(xs => fun(alpha => mapSeq(fun(x => alpha * x), xs)))
  val dot = fun(xs => fun(ys => reduceSeq(add, 0.0f) o mapSeq(mult) $ zip(xs, ys)))

  val high_level =
    fun(matT)(mat => fun(xsT)(xs => fun(ysT)(ys =>
      fun(dataT)(alpha => fun(dataT)(beta =>

        mapSeq(fun(x => x._1 + x._2)) $
          zip(mapSeq(fun(row => alpha * dot(row)(xs)), mat), scal(ys)(beta))

      )))))

  {
    val lambda = TypeInference(high_level, Map()).convertToPhrase
    println("high_level:\n" + PrettyPhrasePrinter(lambda))
    TypeCheck(lambda)
  }

  val fullMatrixVectorFusedOpenCL =
    fun(matT)(mat => fun(xsT)(xs => fun(ysT)(ys =>
      fun(dataT)(alpha => fun(dataT)(beta =>

        join() o mapWorkgroup(fun(t =>
          mapLocal(fun(x => (alpha * x) + (t._2 * beta))) o
            toLocal(mapLocal(reduceSeq(fun(x => fun(a => mult(x) + a)), 0.0f)))
            o split(N) $ zip(xs, t._1)
        )) $ zip(mat, ys)

      )))))

//  runOpenCLKernel("fullMatrixVectorFusedOpenCL", fullMatrixVectorFusedOpenCL)

  val fullMatrixVectorFusedOpenCLAMD =
    fun(matT)(mat => fun(xsT)(xs => fun(ysT)(ys =>
      fun(dataT)(alpha => fun(dataT)(beta =>

        join() o mapWorkgroup(fun(t =>
          mapLocal(fun(x => (alpha * x) + (t._2 * beta))) o
            toLocal(mapLocal(reduceSeq(add, 0.0f))) o split(128) o
            toLocal(mapLocal(reduceSeq(fun(x => fun(a => mult(x) + a)), 0.0f)))
            o split(N /^ 128) o reorderWithStride(128) $ zip(xs, t._1)
        )) $ zip(mat, ys)

      )))))

//  runOpenCLKernel("fullMatrixVectorFusedOpenCLAMD", fullMatrixVectorFusedOpenCLAMD)

  val keplerBest =
    fun(matT)(mat => fun(xsT)(xs => fun(ysT)(ys =>
      fun(dataT)(alpha => fun(dataT)(beta =>

        mapWorkgroup(fun(t =>
          fun(x => (x * alpha) + (t._2 * beta)) o
            toLocal(reduceSeq(add, 0.0f)) o
            toLocal(mapLocal(reduceSeq(fun(x => fun(a => mult(x) + a)), 0.0f))) o
            split(N /^ 128) o reorderWithStride(128) $ zip(xs, t._1)
        )) $ zip(mat, ys)

      )))))

//  runOpenCLKernel("keplerBest", keplerBest)

//  Executor.shutdown()

  {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val fullMatrixVectorFusedOpenCL =
      fun(matT)(mat => fun(xsT)(xs => fun(ysT)(ys =>
        fun(dataT)(alpha => fun(dataT)(beta =>

          join() o mapPar(fun(t =>
            mapSeq(fun(x => (alpha * x) + (t._2 * beta))) o
              toLocal(mapSeq(reduceSeq(fun(x => fun(a => mult(x) + a)), 0.0f)))
              o split(N) $ zip(xs, t._1)
          )) $ zip(mat, ys)

        )))))

    val phrase = TypeInference(fullMatrixVectorFusedOpenCL, Map()).toPhrase
    val program = OpenMP.ProgramGenerator.makeCode(phrase)
    println(program.code)
  }

}
