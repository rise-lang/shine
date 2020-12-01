package rise.core

import arithexpr.arithmetic.NamedVar
import rise.core.traversal.{Continue, Result}
import rise.core.types._

object IsClosedForm {
  private case class FreeVariable(v: Any) extends Exception

  def apply(expr: Expr): Boolean = {
    case class Visitor(
        boundV: Set[Identifier],
        boundT: Set[DataTypeIdentifier],
        boundN: Set[NamedVar],
        boundNatDataTypeFun: Set[NatToDataIdentifier]
    ) extends traversal.Visitor {
      override def visitExpr(e: Expr): Result[Expr] = {
        e match {
          case i: Identifier if !boundV(i) => throw FreeVariable(i)
          case Lambda(x, _) =>
            Continue(e, this.copy(boundV = boundV + x))
          case DepLambda(x: NatIdentifier, _) =>
            Continue(e, this.copy(boundN = boundN + x))
          case DepLambda(x: DataTypeIdentifier, _) =>
            Continue(e, this.copy(boundT = boundT + x))
          case _ => Continue(e, this)
        }
      }

      override def visitNat(ae: Nat): Result[Nat] =
        IsClosedForm.visitNat(ae, boundN, this)

      // TODO: use a single bound: Set[Kind.Identifier]
      override def visitType[T <: Type](t: T): Result[T] = {
        case class TypeVisitor(
            boundT: Set[DataTypeIdentifier],
            boundN: Set[NamedVar],
            boundNatDataTypeFun: Set[NatToDataIdentifier]
        ) extends traversal.Visitor {
          override def visitType[U <: Type](t: U): Result[U] = {
            t match {
              case _: TypeIdentifier => throw FreeVariable(t)
              case DepFunType(x: NatIdentifier, _) =>
                Continue(t, this.copy(boundN = boundN + x))
              case DepFunType(x: DataTypeIdentifier, _) =>
                Continue(t, this.copy(boundT = boundT + x))
              case i: DataTypeIdentifier if !boundT(i) => throw FreeVariable(t)
              case DepArrayType(_, elementTypeFun) =>
                elementTypeFun match {
                  case i: NatToDataIdentifier =>
                    if (!boundNatDataTypeFun(i)) {
                      throw FreeVariable(t)
                    } else {
                      Continue(t, this)
                    }
                  case NatToDataLambda(x, _) =>
                    Continue(t, this.copy(boundN = boundN + x))
                }
              case DepPairType(x, dt) => x match {
                case x:NatIdentifier =>  Continue(t, this.copy(boundN = boundN + x))
                case x:NatCollectionIdentifier => Continue(t, this)
                case _ => ???
              }

              case _ => Continue(t, this)
            }
          }

          override def visitNat(ae: Nat): Result[Nat] =
            IsClosedForm.visitNat(ae, boundN, this)
        }

        traversal.types.DepthFirstLocalResult(
          t,
          TypeVisitor(boundT, boundN, boundNatDataTypeFun)
        )
        Continue(t, this)
      }
    }

    try {
      traversal.DepthFirstLocalResult(expr, Visitor(Set(), Set(), Set(), Set()))
      true
    } catch {
      case FreeVariable(v) =>
        println(s"free variable $v")
        // traversal.DepthFirstLocalResult(expr, TraceType(v, Nil))
        false
    }
  }

  private def visitNat(
      ae: Nat,
      bound: Set[NamedVar],
      v: traversal.Visitor
  ): Result[Nat] = {
    val closed = ae.varList.foldLeft(true) {
      case (c, v: NamedVar) => c && bound(v)
      case (c, _)           => c
    }
    if (closed) {
      Continue(ae, v)
    } else {
      throw FreeVariable(ae)
    }
  }

  private case class TraceType(target: Type, trace: Seq[Expr])
    extends traversal.Visitor
  {
    override def visitExpr(e: Expr): Result[Expr] = {
      Continue(e, TraceType(target, e +: trace))
    }

    override def visitType[T <: Type](t: T): Result[T] =
      traversal.Stop(traversal.types.DepthFirstLocalResult(t,
        new traversal.Visitor {
          override def visitType[U <: Type](t: U): Result[U] = {
            if (t == target) {
              println(trace.headOption.map(_.t))
              println("-- trace --")
              println(trace.mkString("\n"))
              println("----")
              throw new Exception("")
            }
            Continue(t, this)
          }
        }))
  }
}