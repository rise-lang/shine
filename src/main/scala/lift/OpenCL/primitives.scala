package lift.OpenCL

// TODO: move
import idealised.OpenCL.AddressSpace

import lift.core.DSL._
import lift.core.types._
import lift.core.{Expr, primitives => cp}

import scala.language.implicitConversions

object primitives {
  sealed trait Primitive extends lift.core.Primitive

  case class mapGlobal(dim: Int = 0) extends Primitive {
    override def t: Type = cp.map.t
  }

  object mapGlobal {
    def apply(e: Expr): Expr = mapGlobal()(e)
    implicit def toMapGlobal(m: mapGlobal.type): mapGlobal = mapGlobal()
  }

  case class mapLocal(dim: Int = 0) extends Primitive {
    override def t: Type = cp.map.t
  }

  object mapLocal {
    def apply(e: Expr): Expr = mapLocal()(e)
    implicit def toMapLocal(m: mapLocal.type): mapLocal = mapLocal()
  }

  case class mapWorkGroup(dim: Int = 0) extends Primitive {
    override def t: Type = cp.map.t
  }

  object mapWorkGroup {
    def apply(e: Expr): Expr = mapWorkGroup()(e)
    implicit def toMapWorkGroup(m: mapWorkGroup.type): mapWorkGroup = mapWorkGroup()
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
    override def t: Type = cp.reduceSeq.t
  }
}