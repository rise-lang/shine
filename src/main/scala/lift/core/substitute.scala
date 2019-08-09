package lift.core

import lift.core.types._
import lift.arithmetic._
import lift.core.semantics.NatData
import traversal.{Continue, Result, Stop}

object substitute {

  // substitute in Expr

  def kindInExpr[K <: Kind](x: K#T, `for`: K#I, in: Expr): Expr =  (x, `for`) match {
    case (n: Nat, forN: NatIdentifier) => natInExpr(n, forN, in)
    case (dt: DataType, forDt: DataTypeIdentifier) => dataTypeInExpr(dt, forDt, in)
  }

  def exprInExpr(expr: Expr, `for`: Expr, in: Expr): Expr = {
    object Visitor extends traversal.Visitor {
      override def visitExpr(e: Expr): Result[Expr] = {
        if (`for` == e) { Stop(expr) } else { Continue(e, this) }
      }
    }

    traversal.DepthFirstLocalResult(in, Visitor)
  }

  def dataTypeInExpr(dt: DataType,
                     `for`: DataTypeIdentifier,
                     in: Expr): Expr = {

    object Visitor extends traversal.Visitor {
      override def visitType[T <: Type](in: T): Result[T] =
        Continue(substitute.typeInType(dt, `for`, in), this)
    }

    traversal.DepthFirstLocalResult(in, Visitor)
  }

  def natInExpr(ae: Nat,
                `for`: NatIdentifier,
                in: Expr): Expr = {

    object Visitor extends traversal.Visitor {
      override def visitExpr(e: Expr): Result[Expr] = {
        e match {
          case Identifier(name) =>
            if (`for`.name == name) {
              Stop(Literal(NatData(ae)))
            } else {
              Continue(e, this)
            }
          case _ => Continue(e, this)
        }
      }

      override def visitNat(e: Nat): Result[Nat] =
        Continue(substitute.natInNat(ae, `for`, e), this)

      override def visitType[T <: Type](t: T): Result[T] =
        Continue(substitute.natInType(ae, `for`, t), this)
    }

    traversal.DepthFirstLocalResult(in, Visitor)
  }

  // substitute in Type

  def kindInType[K <: Kind, T <: Type](x: K#T, `for`: K#I, in: T): T =  (x, `for`) match {
    case (dt: DataType, forDt: DataTypeIdentifier)        => typeInType(dt, forDt, in)
    case (n: Nat, forN: NatIdentifier)                    => natInType(n, forN, in)
    case (a: AddressSpace, forA: AddressSpaceIdentifier)  => addressSpaceInType(a, forA, in)
    case (n2n: NatToNat, forN2N: NatToNatIdentifier)  => n2nInType(n2n, forN2N, in)
  }

  def typeInType[B <: Type](ty: Type, `for`: Type, in: B): B = {
    case class Visitor() extends traversal.Visitor {
      override def visitType[T <: Type](t: T): traversal.Result[T] = {
        if (`for` == t) {
          Stop(ty.asInstanceOf[T])
        } else {
          Continue(t, this)
        }
      }
    }
    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def natInType[T <: Type](n: Nat, `for`: NamedVar, in: T): T = {
    case class Visitor() extends traversal.Visitor {
      override def visitNat(in: Nat): Result[Nat] =
        Continue(substitute.natInNat(n, `for`, in), this)
    }

    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def natInDataType(n: Nat, `for`: NamedVar, in: DataType): DataType = {
    case class Visitor() extends traversal.Visitor {
      override def visitNat(in: Nat): Result[Nat] =
        Continue(substitute.natInNat(n, `for`, in), this)
    }

    traversal.types.DepthFirstLocalResult.data(in, Visitor())
  }

  def addressSpaceInType[T <: Type](a: AddressSpace, `for`: AddressSpaceIdentifier, in: T): T = {
    case class Visitor() extends traversal.Visitor {
      override def visitAddressSpace(b: AddressSpace): Result[AddressSpace] =
        if (`for` == b) {
          Stop(a)
        } else {
          Continue(b, this)
        }
    }

    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def n2nInType[T <: Type](n2n: NatToNat, `for`: NatToNatIdentifier, in: T): T = {
    case class Visitor() extends traversal.Visitor {
      override def visitN2N(n: NatToNat): Result[NatToNat] =
        if (`for` == n) {
          Stop(n2n)
        } else {
          Continue(n, this)
        }
    }
    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  def n2dInType[T <: Type](n2d: NatToData, `for`: NatToDataIdentifier, in: T): T = {
    case class Visitor() extends traversal.Visitor {
      override def visitN2D(n: NatToData): Result[NatToData] =
        if (`for` == n) {
          Stop(n2d)
        } else {
          Continue(n, this)
        }
    }
    traversal.types.DepthFirstLocalResult(in, Visitor())
  }

  // substitute in Nat

  def natInNat(ae: Nat, `for`: NamedVar, in: Nat): Nat = {
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

  // substitute in AddressSpace

  def addressSpaceInAddressSpace(a: AddressSpace, `for`: AddressSpaceIdentifier, in: AddressSpace): AddressSpace = {
    if (in == `for`) { a } else { in }
  }

  // substitute in NatToData

  def n2dInN2d(n: NatToData, `for`: NatToData, in: NatToData): NatToData = {
    in match {
      case i: NatToDataIdentifier => if (i == `for`) n else in
      case _ => in
    }
  }
}