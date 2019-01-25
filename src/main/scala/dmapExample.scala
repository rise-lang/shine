import idealised.OpenCL.KernelGenerator
import idealised.OpenCL.SurfaceLanguage.DSL.{depMapGlobal, depMapLocal, depMapWorkgroup}
import idealised.OpenMP.SurfaceLanguage.DSL.depMapPar
import idealised.SurfaceLanguage.DSL._
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

  val addOne = fun(xsT)(array => depMapSeq(fun(x => mapSeq(fun(y => y + 1), x) ), array))

  val reduceByRow = fun(xsT)(array => depMapSeq(fun(x => reduceSeq(fun(y => fun(z => y + z)), 0, x) ), array))

  val takeAndDrop = fun(ArrayType(8, int))(array => mapSeq(fun(x => x), take(2, drop(3, array))))

  val sliceTest = fun(ArrayType(8, int))(array => mapSeq(fun(x => x), array :>> slice(3, 2) ))

  val mult = fun(x => x._1 * x._2)

  val add = fun(x => fun(y => x + y))

  val addOneGlobal = fun(xsT)(array => depMapGlobal(fun(x => mapSeq(fun(y => y + 1), x) ), array))

  val addOneWorkgroup = fun(xsT)(array => depMapWorkgroup(fun(x => mapSeq(fun(y => y + 1), x) ), array))

  val triangleVectorMultSeq: Expr[DataType -> (DataType -> DataType)] =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMapSeq(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> mapSeq(mult) :>> reduceSeq(add, 0)
        ), triangle)
      )
    )

  val triangleVectorMultPar: Expr[DataType -> (DataType -> DataType)] =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMapPar(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> mapSeq(mult) :>> reduceSeq(add, 0)
        ), triangle)
      )
    )

  val fInUse = addOneWorkgroup

  val typed_f = TypeInference(fInUse, Map())

  typed_f.t

  printKernel(fInUse)

  def printKernel[T <: Type](expr: Expr[T]) {
    //def generate(e:Expr[T]) = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(e, Map()).toPhrase)
    def generate(e:Expr[T]) = KernelGenerator.makeCode(TypeInference(e, Map()).toPhrase, localSize = 8, globalSize = 8)
    println(generate(expr).code)
  }
}
