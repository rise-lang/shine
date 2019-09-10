import idealised.DPIA
import idealised.OpenCL._
import idealised.OpenCL.KernelGenerator
import idealised.OpenCL.SurfaceLanguage.DSL.{depMapGlobal, depMapWorkgroup}
import idealised.OpenMP.SurfaceLanguage.DSL.depMapPar
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import opencl.executor.Executor

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

  val triangleVectorMultSeq: Expr =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMapSeq(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> mapSeq(mult) :>> reduceSeq(add, 0)
        ), triangle)
      )
    )

  val triangleVectorMultPar: Expr =
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

  def printKernel(expr: Expr) {
    //def generate(e:Expr) = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(e, Map()).toPhrase)
    def generate(e:Expr) =
      KernelGenerator.makeCode(LocalSize(8), GlobalSize(8))(DPIA.FromSurfaceLanguage(TypeInference(e, Map())), "KERNEL")
    println(generate(expr).code)
  }

  val multSumAcc = fun(x => fun(y => (x._1 * x._2) + y))

  def triangleVectorMultGlobalFused(N:Nat): Expr =
    fun(DepArrayType(N, i => ArrayType(i + 1, float)))(triangle =>
      fun(ArrayType(N, float))(vector =>
        depMapGlobal(fun(row => zip(row, take(Macros.GetLength(row), vector)) :>> reduceSeq(multSumAcc, 0.0f)
        ), triangle)
      )
    )
  val actualN = 64


  Executor.loadAndInit()
  import idealised.OpenCL._
  val kernel = idealised.OpenCL.KernelGenerator
    .makeCode(LocalSize(1), GlobalSize(1)) (DPIA.FromSurfaceLanguage(TypeInference(triangleVectorMultGlobalFused(actualN), Map())),
                                            "KERNEL")
  println(kernel.code)

  val inputVector = Array.tabulate(actualN)(id => id + 1.0f)
  val inputMatrix = Array.tabulate(actualN)(rowIndex => Array.tabulate(rowIndex + 1)(colIndex => if(colIndex == rowIndex) 1.0f else 0.0f))

  val kernelFun = kernel.as[ScalaFunction `(` Array[Array[Float]] `,` Array[Float] `)=>` Array[Float]]

  val (output, time) = kernelFun(inputMatrix `,` inputVector)

  output.foreach(x => print(x + " "))
  Executor.shutdown()
}
