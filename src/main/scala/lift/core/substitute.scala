package lift.core

import lift.core.types._
import lift.arithmetic._
import traversal.{Result, Stop, Continue}

object substitute {
  def apply[K <: Kind](x: K#T, `for`: K#I, in: Expr): Expr =  (x, `for`) match {
    case (n: Nat, forN: NatIdentifier) => apply(n, forN, in)
    case (dt: DataType, forDt: DataTypeIdentifier) => apply(dt, forDt, in)
  }


  def apply(expr: Expr, `for`: Expr, in: Expr): Expr = {
    object Visitor extends traversal.Visitor {
      override def apply(e: Expr): Result[Expr] = {
        if (`for` == e) { Stop(expr) } else { Continue(e, this) }
      }
    }

    traversal.DepthFirstLocalResult(in, Visitor)
  }

  def apply(dt: DataType,
            `for`: DataTypeIdentifier,
            in: Expr): Expr = {

    object Visitor extends traversal.Visitor {
      override def apply[T <: Type](in: T): Result[T] =
        Continue(substitute(dt, `for`, in), this)
    }

    traversal.DepthFirstLocalResult(in, Visitor)
  }

  def apply(ae: Nat,
            `for`: NatIdentifier,
            in: Expr): Expr = {

    object Visitor extends traversal.Visitor {
      override def apply(e: Expr): Result[Expr] = {
        e match {
          case Identifier(name) =>
            if (`for`.name == name) {
              Stop(NatExpr(ae))
            } else {
              Continue(e, this)
            }
          case _ => Continue(e, this)
        }
      }

      override def apply(e: Nat): Result[Nat] =
        Continue(substitute(ae, `for`, e), this)

      override def apply[T <: Type](t: T): Result[T] =
        Continue(substitute(ae, `for`, t), this)
    }

    traversal.DepthFirstLocalResult(in, Visitor)
  }

  def apply[K <: Kind, T <: Type](x: K#T, `for`: K#I, in: T): T =  (x, `for`) match {
    case (dt: DataType, forDt: DataTypeIdentifier)        => apply(dt, forDt, in)
    case (n: Nat, forN: NatIdentifier)                    => apply(n, forN, in)
    case (a: AddressSpace, forA: AddressSpaceIdentifier)  => apply(a, forA, in)
    case (a: AccessType, forA: AccessTypeIdentifier)      => apply(a, forA, in)
  }

  def apply[A <: Type, B <: Type](ty: A, `for`: A, in: B): B = {
    case class Visitor() extends traversal.Visitor {
      override def apply[T <: Type](t: T): traversal.Result[T] = {
        if (`for` == t) {
          Stop(ty.asInstanceOf[T])
        } else {
          Continue(t, this)
        }
      }
    }

    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def apply[T <: Type](n: Nat, `for`: NamedVar, in: T): T = {
    case class Visitor() extends traversal.Visitor {
      override def apply(in: Nat): Result[Nat] =
        Continue(substitute(n, `for`, in), this)
    }

    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def apply[T <: Type](a: AddressSpace, `for`: AddressSpaceIdentifier, in: T): T = {
    case class Visitor() extends traversal.Visitor {
      override def apply(b: AddressSpace): Result[AddressSpace] =
        if (`for` == b) {
          Stop(a)
        } else {
          Continue(b, this)
        }
    }

    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def apply[T <: Type](a: AccessType, `for`: AccessTypeIdentifier, in: T): T = {
    case class Visitor() extends traversal.Visitor {
      override def apply(b: AccessType): Result[AccessType] =
        if (`for` == b) {
          Stop(a)
        } else {
          Continue(b, this)
        }
    }

    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def apply(ae: Nat, `for`: NamedVar, in: Nat): Nat = {
    in.visitAndRebuild {
      case v: NamedVar =>
        if (`for`.name == v.name) {
          ae
        } else {
          v
        }
      case e => e
    }
  }
}