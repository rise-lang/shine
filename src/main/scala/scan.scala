import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.OpenCL._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import opencl.executor.Executor

import scala.language.{implicitConversions, postfixOps}

/**
  * Created by federico on 13/01/18.
  */
object scan extends App{
  Executor.loadLibrary()
  Executor.init()


  val xsT = ArrayType(8, float)
  val mult = fun(x => x._1 * x._2)

  val basicScan = fun(xsT)(array => scanSeq(fun(x => fun(a => a + x)), 0.0f, array))

  printKernel(basicScan)

  def printKernel(expr: Expr[DataType -> DataType]) {
    val kernel = KernelGenerator.makeCode(TypeInference(expr, Map()).toPhrase, localSize = 8, globalSize = 8)
    println(kernel.code)
  }
}
