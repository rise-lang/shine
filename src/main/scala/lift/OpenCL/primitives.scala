package lift.OpenCL

import lift.core.DSL._
import lift.core.types._
import lift.core.{Expr, primitives => core}

import scala.language.implicitConversions

object primitives {
  sealed trait Primitive extends lift.core.Primitive

  // TODO? depMapGlobal, depMapLocal, depMapWorkGroup

  case class mapGlobal(dim: Int) extends Primitive {
    override def t: Type = core.map.t
  }

  object mapGlobal {
    def apply(): mapGlobal = mapGlobal(0)
    def apply(e: Expr): Expr = mapGlobal(0)(e)

    implicit def toMapGlobal(m: mapGlobal.type): mapGlobal = mapGlobal(0)
  }

  case class mapLocal(dim: Int) extends Primitive {
    override def t: Type = core.map.t
  }

  object mapLocal {
    def apply(): mapLocal = mapLocal(0)
    def apply(e: Expr): Expr = mapLocal(0)(e)

    implicit def toMapLocal(m: mapLocal.type): mapLocal = mapLocal(0)
  }

  case class mapWorkGroup(dim: Int) extends Primitive {
    override def t: Type = core.map.t
  }

  object mapWorkGroup {
    def apply(): mapWorkGroup = mapWorkGroup(0)
    def apply(e: Expr): Expr = mapWorkGroup(0)(e)

    implicit def toMapLocal(m: mapWorkGroup.type): mapWorkGroup = mapWorkGroup(0)
  }


  object toMem extends Primitive {
    override def t: Type = implDT(t => aFunT(a => t ->: t))
  }

  def toFun(to: Expr, f: Expr): Expr = fun(x => to(f(x)))

  val toGlobal: Expr = toMem(lift.core.types.AddressSpace.Global)
  def toGlobalFun(f: Expr): Expr = toFun(toGlobal, f)
  val toLocal: Expr = toMem(lift.core.types.AddressSpace.Local)
  def toLocalFun(f: Expr): Expr = toFun(toLocal, f)
  val toPrivate: Expr = toMem(lift.core.types.AddressSpace.Private)
  def toPrivateFun(f: Expr): Expr = toFun(toPrivate, f)

  object oclReduceSeq extends Primitive {
    override def t: Type = aFunT(a => core.reduceSeq.t)
  }

  object oclIterate extends Primitive {
    override def t: Type = aFunT(a => implN(n => implN(m => nFunT(k => implDT(t =>
      nFunT(l => ArrayType(l * n, t) ->: ArrayType(l, t)) ->:
        ArrayType(m * n.pow(k), t) ->: ArrayType(m, t)
    )))))
  }
}
