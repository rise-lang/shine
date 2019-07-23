package lift.core

import lift.core.types._
import lift.arithmetic._
import traversal.{Result, Stop, Continue}

object substitute {

  // substitute in Expr

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

  // substitute in Type

  def apply[K <: Kind, T <: Type](x: K#T, `for`: K#I, in: T): T =  (x, `for`) match {
    case (n: Nat, forN: NatIdentifier)                => apply(n, forN, in)
    case (dt: DataType, forDt: DataTypeIdentifier)    => apply(dt, forDt, in)
    case (n2n: NatToNat, forN2N: NatToNatIdentifier)  => apply(n2n, forN2N, in)
  }

  def apply[T <: Type](ae: Nat, `for`: NamedVar, in: T): T = {
    case class Visitor() extends traversal.Visitor {
      override def apply(n: Nat): Result[Nat] =
        Continue(substitute(ae, `for`, n), this)
    }

    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def apply[A <: Type, B <: Type](ty: A, `for`: A, in: B): B = {
    case class Visitor() extends traversal.Visitor {
      override def apply[T <: Type](t: T): traversal.Result[T] =
        t match {
          case NatToDataApply(n2d1, x) => `for` match {
            case NatToDataApply(n2d2, y: NamedVar) if n2d1 == n2d2 =>
                Stop(substitute(x, y, ty).asInstanceOf[T])
            case _ => Continue(t, this)
          }
          case _ if `for` == t => Stop(ty.asInstanceOf[T])
          case _ => Continue(t, this)
        }
    }
    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def apply[T <: Type](n2n: NatToNat, `for`: NatToNatIdentifier, in: T): T = {
    case class Visitor() extends traversal.Visitor {
      override def apply(n: NatToNat): Result[NatToNat] =
        if (`for` == n) {
          Stop(n2n)
        } else {
          Continue(n, this)
        }
    }
    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def apply[T <: Type](n2d: NatToData, `for`: NatToDataIdentifier, in: T): T = {
    case class Visitor() extends traversal.Visitor {
      override def apply(n: NatToData): Result[NatToData] =
        if (`for` == n) {
          Stop(n2d)
        } else {
          Continue(n, this)
        }
    }
    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  // substitute in Nat

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

  // substitute in NatToData

  def apply(n: NatToData, `for`: NatToData, in: NatToData): NatToData = {
    in match {
      case i: NatToDataIdentifier => if (i == `for`) n else in
      case _ => in
    }
  }
}