package idealised.OpenCL.SurfaceLanguage.DSL

import idealised.OpenCL.AddressSpace
import idealised.OpenCL.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.DSL.{nFun, fun}
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, _}

import scala.language.reflectiveCalls

//noinspection TypeAnnotation
object mapGlobal {
  def apply(f: Expr): Expr = mapGlobal(0)(f)
  def apply(f: Expr, x: Expr): MapGlobal = mapGlobal(0)(f, x)

  def apply(dim: Int) = new {
    def apply(f: Expr): Expr =
      fun(x => MapGlobal(dim)(f, x))

    def apply(f: Expr, x: Expr): MapGlobal =
      MapGlobal(dim)(f, x)
  }
}

//noinspection TypeAnnotation
object depMapGlobal {
  def apply(f: Expr): Expr = depMapGlobal(0)(f)
  def apply(f: Expr, x: Expr): DepMapGlobal = depMapGlobal(0)(f, x)

  def withIndex(f: Expr): Expr = fun(x => withIndex(f,x))
  def withIndex(f: Expr, x:Expr): DepMapGlobal= DepMapGlobal(0)(f, x)


  def apply(dim: Int) = new {
    def apply(f: Expr): Expr =
      fun(x => DepMapGlobal(dim)(nFun(_ => f), x))

    def apply(f: Expr, x: Expr): DepMapGlobal =
      DepMapGlobal(dim)(nFun(_ => f), x)
  }
}

//noinspection TypeAnnotation
object mapWorkgroup {
  def apply(f: Expr): Expr = mapWorkgroup(0)(f)
  def apply(f: Expr, x: Expr): MapWorkGroup = mapWorkgroup(0)(f, x)

  def apply(dim: Int) = new {
    def apply(f: Expr): Expr =
      fun(x => MapWorkGroup(dim)(f, x))

    def apply(f: Expr, x: Expr): MapWorkGroup =
      MapWorkGroup(dim)(f, x)
  }
}

object depMapWorkgroup {
  def apply(f: Expr): Expr = depMapWorkgroup(0)(f)
  def apply(f: Expr, x: Expr): DepMapWorkGroup = depMapWorkgroup(0)(f, x)

  def withIndex(f: Expr): Expr = fun(x => withIndex(f,x))
  def withIndex(f: Expr, x:Expr): DepMapWorkGroup= DepMapWorkGroup(0)(f, x)

  def apply(dim: Int) = new {
    def apply(f: Expr): Expr =
      fun(x => DepMapWorkGroup(dim)(nFun(_ => f), x))

    def apply(f: Expr, x: Expr): DepMapWorkGroup =
      DepMapWorkGroup(dim)(nFun(_ => f), x)
  }
}

//noinspection TypeAnnotation
object mapLocal {
  def apply(f: Expr): Expr = mapLocal(0)(f)
  def apply(f: Expr, x: Expr): MapLocal = mapLocal(0)(f, x)

  def withIndex(f: Expr): Expr = fun(x => withIndex(f,x))
  def withIndex(f: Expr, x:Expr): DepMapLocal= DepMapLocal(0)(f, x)

  def apply(dim: Int) = new {
    def apply(f: Expr): Expr =
      fun(x => MapLocal(dim)(f, x))

    def apply(f: Expr, x: Expr): MapLocal =
      MapLocal(dim)(f, x)
  }
}

object depMapLocal {
  def apply(f: Expr): Expr = depMapLocal(0)(f)
  def apply(f: Expr, x: Expr): DepMapLocal = depMapLocal(0)(f, x)


  def apply(dim: Int) = new {
    def apply(f: Expr): Expr =
      fun(x => DepMapLocal(dim)(nFun(_ => f), x))

    def apply(f: Expr, x: Expr): DepMapLocal =
      DepMapLocal(dim)(nFun(_ => f), x)
  }
}


object toLocal {
  def apply(f: Expr): Expr =
    fun(x => toLocal(f, x))

  def apply(f: Expr, x: Expr): ToLocal =
    ToLocal(f, x)
}

object toGlobal {
  def apply(f: Expr): Expr =
    fun(x => toGlobal(f, x))

  def apply(f: Expr, x: Expr): ToGlobal =
    ToGlobal(f, x)
}

object toPrivate {
  def apply(f: Expr): Expr =
    fun(x => toPrivate(f, x))

  def apply(f: Expr, x: Expr): ToPrivate =
    ToPrivate(f, x)
}

object oclReduceSeq {
  /* TODO how can we do this?
  def apply(f: Expr): Expr =
    fun((init, array) => reduceSeq(f, init, array))
  */

  def apply(f: Expr,
            init: Expr,
            initAddrSpace: AddressSpace): Expr =
    fun(array => oclReduceSeq(f, init, initAddrSpace, array))

  def apply(f: Expr,
            init: Expr,
            initAddrSpace: AddressSpace,
            array: Expr): OpenCLReduceSeq =
    OpenCLReduceSeq(f, init, initAddrSpace, array, None)
}

object oclFun {
  def apply(name: String, inT: DataType, outT: DataType, arg: Expr): OpenCLFunction =
    OpenCLFunction(name, Seq(inT), outT, Seq(arg))

  def apply(name: String, inTs: Seq[DataType], outT: DataType, args: Seq[Expr]): OpenCLFunction =
    OpenCLFunction(name, inTs, outT, args)
}
