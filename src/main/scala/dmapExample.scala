import idealised.OpenCL.SurfaceLanguage.DSL.reduceSeq
import idealised.OpenCL._
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

  val addOne = fun(xsT)(array => DepMap(dFun(_ => fun(x => map(fun(y => y + 1), x) )), array))

  val reduceByRow = fun(xsT)(array => DepMap(dFun(_ => fun(x => reduceSeq(fun(y => fun(z => y + z)), 0, x) )), array))

  val takeAndDrop = fun(ArrayType(8, int))(array => map(fun(x => x), take(2, drop(3, array))))

  val fInUse = takeAndDrop

  val typed_f = TypeInference(fInUse, Map())

  typed_f.t

  printKernel(fInUse)

  def printKernel(expr: Expr[DataType -> DataType]) {
    val kernel = KernelGenerator.makeKernel(TypeInference(expr, Map()).toPhrase, localSize = 8, globalSize = 8)
    println(kernel.code)
  }
}
