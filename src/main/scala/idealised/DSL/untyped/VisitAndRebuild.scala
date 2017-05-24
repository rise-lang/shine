package idealised.DSL.untyped

import idealised.Core._

import idealised.Core.OperationalSemantics.IndexData

object VisitAndRebuild {

  class Visitor {
    def apply[T <: PhraseType](e: Expr[T]): Result[Expr[T]] = Continue(e, this)
    def apply(ae: Nat): Nat = ae
    def apply[T <: DataType](dt: T): T = dt

    abstract class Result[+T]
    case class Stop[T <: PhraseType](p: Expr[T]) extends Result[Expr[T]]
    case class Continue[T <: PhraseType](p: Expr[T], v: Visitor) extends Result[Expr[T]]
  }

  def apply[T <: PhraseType](e: Expr[T], v: Visitor): Expr[T] = {
    v(e) match {
      case r: v.Stop[T]@unchecked => r.p
      case c: v.Continue[T]@unchecked =>
        val v = c.v
        (c.p match {
          case i: IdentifierExpr =>
            IdentifierExpr(i.name, i.`type` match {
              case None => None
              case Some(dt) => Some(v(dt))
            })

          case LambdaExpr(x, p) =>
            apply(x, v) match {
              case newX: IdentifierExpr => LambdaExpr(newX, apply(p, v))
              case _ => throw new Exception("This should not happen")
            }

          case ApplyExpr(p, q) =>
            ApplyExpr(apply(p, v), apply(q, v))

          case NatDependentLambdaExpr(a, p) =>
            NatDependentLambdaExpr(a, apply(p, v))

          case TypeDependentLambdaExpr(dt, p) =>
            TypeDependentLambdaExpr(dt, apply(p, v))

          case NatDependentApplyExpr(p, ae) =>
            NatDependentApplyExpr(apply(p, v), ae)

          case TypeDependentApplyExpr(p, dt) =>
            TypeDependentApplyExpr(apply(p, v), dt)

          case IfThenElseExpr(cond, thenP, elseP) =>
            IfThenElseExpr(apply(cond, v), apply(thenP, v), apply(elseP, v))

          case LiteralExpr(d, t) => d match {
            case IndexData(i) => LiteralExpr(IndexData(v(i)), v(t))
            case _ => LiteralExpr(d, v(t))
          }

          case UnaryOpExpr(op, x) => UnaryOpExpr(op, apply(x, v))

          case BinOpExpr(op, lhs, rhs) => BinOpExpr(op, apply(lhs, v), apply(rhs, v))

          case p: PrimitiveExpr => p.visitAndRebuild(v)
        }).asInstanceOf[Expr[T]]
    }
  }

}

