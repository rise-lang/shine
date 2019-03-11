package idealised.OpenCL.SurfaceLanguage.DSL

import idealised.OpenCL.AddressSpace
import idealised.OpenCL.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.DSL.{DataExpr, nFun, fun}
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, _}

import scala.language.reflectiveCalls

//noinspection TypeAnnotation
object mapGlobal {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = mapGlobal(0)(f)
  def apply(f: Expr[DataType -> DataType], x: DataExpr): MapGlobal = mapGlobal(0)(f, x)

  def apply(dim: Int) = new {
    def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
      fun(x => MapGlobal(dim)(f, x))

    def apply(f: Expr[DataType -> DataType], x: DataExpr): MapGlobal =
      MapGlobal(dim)(f, x)
  }
}

//noinspection TypeAnnotation
object depMapGlobal {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = depMapGlobal(0)(f)
  def apply(f: Expr[DataType -> DataType], x: DataExpr): DepMapGlobal = depMapGlobal(0)(f, x)

  def withIndex(f: Expr[`(nat)->`[DataType -> DataType]]): Expr[DataType -> DataType] = fun(x => withIndex(f,x))
  def withIndex(f: Expr[`(nat)->`[DataType -> DataType]], x:DataExpr): DepMapGlobal= DepMapGlobal(0)(f, x)


  def apply(dim: Int) = new {
    def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
      fun(x => DepMapGlobal(dim)(nFun(_ => f), x))

    def apply(f: Expr[DataType -> DataType], x: DataExpr): DepMapGlobal =
      DepMapGlobal(dim)(nFun(_ => f), x)
  }
}

//noinspection TypeAnnotation
object mapWorkgroup {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = mapWorkgroup(0)(f)
  def apply(f: Expr[DataType -> DataType], x: DataExpr): MapWorkGroup = mapWorkgroup(0)(f, x)

  def apply(dim: Int) = new {
    def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
      fun(x => MapWorkGroup(dim)(f, x))

    def apply(f: Expr[DataType -> DataType], x: DataExpr): MapWorkGroup =
      MapWorkGroup(dim)(f, x)
  }
}

object depMapWorkgroup {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = depMapWorkgroup(0)(f)
  def apply(f: Expr[DataType -> DataType], x: DataExpr): DepMapWorkGroup = depMapWorkgroup(0)(f, x)

  def withIndex(f: Expr[`(nat)->`[DataType -> DataType]]): Expr[DataType -> DataType] = fun(x => withIndex(f,x))
  def withIndex(f: Expr[`(nat)->`[DataType -> DataType]], x:DataExpr): DepMapWorkGroup= DepMapWorkGroup(0)(f, x)

  def apply(dim: Int) = new {
    def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
      fun(x => DepMapWorkGroup(dim)(nFun(_ => f), x))

    def apply(f: Expr[DataType -> DataType], x: DataExpr): DepMapWorkGroup =
      DepMapWorkGroup(dim)(nFun(_ => f), x)
  }
}

//noinspection TypeAnnotation
object mapLocal {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = mapLocal(0)(f)
  def apply(f: Expr[DataType -> DataType], x: DataExpr): MapLocal = mapLocal(0)(f, x)

  def withIndex(f: Expr[`(nat)->`[DataType -> DataType]]): Expr[DataType -> DataType] = fun(x => withIndex(f,x))
  def withIndex(f: Expr[`(nat)->`[DataType -> DataType]], x:DataExpr): DepMapLocal= DepMapLocal(0)(f, x)

  def apply(dim: Int) = new {
    def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
      fun(x => MapLocal(dim)(f, x))

    def apply(f: Expr[DataType -> DataType], x: DataExpr): MapLocal =
      MapLocal(dim)(f, x)
  }
}

object depMapLocal {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = depMapLocal(0)(f)
  def apply(f: Expr[DataType -> DataType], x: DataExpr): DepMapLocal = depMapLocal(0)(f, x)


  def apply(dim: Int) = new {
    def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
      fun(x => DepMapLocal(dim)(nFun(_ => f), x))

    def apply(f: Expr[DataType -> DataType], x: DataExpr): DepMapLocal =
      DepMapLocal(dim)(nFun(_ => f), x)
  }
}


object toLocal {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
    fun(x => toLocal(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): ToLocal =
    ToLocal(f, x)
}

object toGlobal {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
    fun(x => toGlobal(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): ToGlobal =
    ToGlobal(f, x)
}

object toPrivate {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
    fun(x => toPrivate(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): ToPrivate =
    ToPrivate(f, x)
}

object oclReduceSeq {
  /* TODO how can we do this?
  def apply(f: Expr[DataType -> (DataType -> DataType)]): Expr[DataType -> (DataType -> DataType)] =
    fun((init, array) => reduceSeq(f, init, array))
  */

  def apply(f: Expr[DataType -> (DataType -> DataType)],
            init: DataExpr,
            initAddrSpace: AddressSpace): Expr[DataType -> DataType] =
    fun(array => oclReduceSeq(f, init, initAddrSpace, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)],
            init: DataExpr,
            initAddrSpace: AddressSpace,
            array: DataExpr): OpenCLReduceSeq =
    OpenCLReduceSeq(f, init, initAddrSpace, array, None)
}

object oclFun {
  def apply(name: String, inT: DataType, outT: DataType, arg: DataExpr): OpenCLFunction =
    OpenCLFunction(name, Seq(inT), outT, Seq(arg))

  def apply(name: String, inTs: Seq[DataType], outT: DataType, args: Seq[DataExpr]): OpenCLFunction =
    OpenCLFunction(name, inTs, outT, args)
}
