package idealised.apps

import idealised.DPIA.FunctionalPrimitives.WithIndex
import idealised.SurfaceLanguage.{->, Expr}
import idealised.SurfaceLanguage.Types.DataType
import idealised.OpenCL.SurfaceLanguage.DSL.{depMapGlobal, depMapWorkgroup, mapLocal}
import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, `,`}
import idealised.OpenMP.SurfaceLanguage.DSL.depMapPar
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.util.SyntaxChecker
import lift.arithmetic._
import opencl.executor.Executor

class bandedVector extends idealised.util.TestsWithExecutor {

  val add = fun(x => fun(y => x + y))

  val multSumAcc = fun(x => fun(y => (x._1 * x._2) + y))

  def bandedMatrixTVector(kl:Int, ku:Int): Expr[DataType -> (DataType -> DataType)] = {
    val LDA = Cst(kl + ku + 1)
    val N = NamedVar("N", ContinuousRange(0, 100000))
    val M = NamedVar("M", ContinuousRange(0, 100000))
    val KL = NamedVar("KL", ContinuousRange(0, 100))
    val KU = NamedVar("KU", ContinuousRange(0, 100))
    fun(ArrayType(LDA, ArrayType(N, float)))(bandedMatrix =>
      fun(ArrayType(N, float))(vector => {
        bandedMatrix :>> transpose() :>> withIndex() :>> depMapSeq.withIndex(dFun(rowIndex =>
          fun(row => {
            import ArithExpr._
            val skip = max(0, ku - rowIndex)
            val drop = max(0, kl - (N - rowIndex))
            val elementsTaken = LDA - skip - drop
            zip(slice(skip, elementsTaken)(row), slice(rowIndex, elementsTaken)(vector)) :>> reduceSeq(multSumAcc, 0.0f)
            //vector :>> slice(rowIndex, elementsTaken) :>> reduceSeq(add, 0.0f)
          })))
      })
    )
  }

  def toBandedReprs(matrix:Array[Array[Float]], n:Int, m:Int, kl:Int, ku:Int):Array[Array[Float]] = {
    val outputArray = Array.fill(kl + ku + 1, n)(-1.0f)

    for {
      j <- 0 until n
      i <- 0 until m
      if  i==j ||  (j - i <= ku && j>i) || (i - j <= kl && i>j)
    } {
      outputArray(ku+i-j)(j) = matrix(i)(j)
    }

    outputArray
  }

  test("Banded matrix vector") {
    import idealised.OpenCL._


    val n = 5
    val originalKL = 2
    val originalKU = 1
    val originalMatrix = Array(
      Array(1.1f, 1.2f, -1.0f, -1.0f, 0.0f),
      Array(2.1f, 2.2f, 2.3f, -1.0f, 0.0f),
      Array(3.1f, 3.2f, 3.3f, 3.4f, 0.0f),
      Array(-1.0f, 4.2f, 4.3f, 4.4f, 4.5f),
      Array(-1.0f, -1.0f, 5.3f, 5.4f, 5.5f)
    )

    val originalMatrixIdentity = Array(
      Array(1.0f, 0.0f, 0.0f, 0.0f, 0.0f),
      Array(0.0f, 1.0f, 0.0f, 0.0f, 0.0f),
      Array(0.0f, 0.0f, 1.0f, 0.0f, 0.0f),
      Array(0.0f, 0.0f, 0.0f, 1.0f, 0.0f),
      Array(0.0f, 0.0f, 0.0f, 0.0f, 1.0f)
    )

    val bandedOriginal = toBandedReprs(originalMatrixIdentity, n = 5, m = 5, kl = originalKL, ku = originalKU)

    val transposedMatrix = originalMatrixIdentity.transpose

    //This is the banded representation of the transposed matrix, so kl <-> ku need to be flipped with respect to the input data
    val transposedMatrixBanded = toBandedReprs(transposedMatrix, n = 5, m = 5, kl = originalKU, ku = originalKL)

    val transposedLikeInsideKernel = transposedMatrixBanded.transpose
    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(bandedMatrixTVector(kl = originalKU, ku = originalKL), Map()).toPhrase, 1, 1)
    println(kernel.code)

    val kernelFun = kernel.as[ScalaFunction `(` Array[Array[Float]] `,` Array[Float] `)=>` Array[Float]]
    val (output, time) = kernelFun(transposedMatrixBanded `,` Array.tabulate(n)(i => i + 1.0f))

    output.foreach(println)
  }
}
