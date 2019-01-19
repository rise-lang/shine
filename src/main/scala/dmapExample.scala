import idealised.OpenCL.SurfaceLanguage.DSL.{depMap, reduceSeq}
import idealised.OpenMP.SurfaceLanguage.DSL.depMapPar
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Primitives.DepMap
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

import scala.language.{implicitConversions, postfixOps}

/**
  * Created by federico on 13/01/18.
  */
object dmapExample extends App{
//  Executor.loadLibrary()
//  Executor.init()


  val xsT = DepArrayType(8, i => ArrayType(i + 1, int))

  val addOne = fun(xsT)(array => DepMap(dFun(_ => fun(x => mapSeq(fun(y => y + 1), x) )), array))

  val reduceByRow = fun(xsT)(array => depMap(fun(x => reduceSeq(fun(y => fun(z => y + z)), 0, x) ), array))

  val takeAndDrop = fun(ArrayType(8, int))(array => mapSeq(fun(x => x), take(2, drop(3, array))))

  val sliceTest = fun(ArrayType(8, int))(array => mapSeq(fun(x => x), array :>> slice(3, 2) ))


  val mult = fun(x => x._1 * x._2)

  val add = fun(x => fun(y => x + y))

  val triangleVectorMult: Expr[DataType -> (DataType -> DataType)] =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMap(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> mapSeq(mult) :>> reduce(add, 0)
        ), triangle)
      )
    )

  val parTriangleVectorMult: Expr[DataType -> (DataType -> DataType)] =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMapPar(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> mapSeq(mult) :>> reduce(add, 0)
        ), triangle)
      )
    )

  val fInUse = parTriangleVectorMult

  val typed_f = TypeInference(fInUse, Map())

  typed_f.t

  printKernel(fInUse)

  def printKernel(expr: Expr[DataType -> (DataType -> DataType)]) {
    val kernel = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(expr, Map()).toPhrase)
    //val kernel = KernelGenerator.makeKernel(TypeInference(expr, Map()).toPhrase, localSize = 8, globalSize = 8)
    println(kernel.code)
  }
}
