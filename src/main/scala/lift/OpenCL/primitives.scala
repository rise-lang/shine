package lift.OpenCL

// TODO: move
import idealised.OpenCL.AddressSpace

import lift.core.DSL._
import lift.core.types._
import lift.core.{Expr, primitives => core}

import scala.language.implicitConversions

object primitives {
  sealed trait Primitive extends lift.core.Primitive

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


  case class to(space: AddressSpace) extends Primitive {
    override def t: Type = implT(a => implT(b =>
      (a -> b) -> (a -> b)
    ))
  }

  val toGlobal = to(idealised.OpenCL.GlobalMemory)
  val toLocal = to(idealised.OpenCL.LocalMemory)
  val toPrivate = to(idealised.OpenCL.PrivateMemory)

  case class oclReduceSeq(init_space: AddressSpace) extends Primitive {
    override def t: Type = core.reduceSeq.t
  }

}
