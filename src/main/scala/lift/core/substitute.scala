package lift.core

import lift.core.types._
import lift.arithmetic._
import traversal.{Result, Stop, Continue}

object substitute {
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

  def apply[T <: Type](ae: Nat, `for`: NatIdentifier, in: T): T = {
    case class Visitor() extends traversal.Visitor {
      override def apply(n: Nat): Result[Nat] =
        Continue(substitute(ae, `for`, n), this)
    }

    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def apply(ae: Nat, `for`: NatIdentifier, in: Nat): Nat = {
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