package idealised.DSL.untyped

import idealised.Core._

sealed trait Expr[T <: PhraseType]

//sealed trait DataExpr extends Expr[ExpType]

final case class IdentifierExpr(name: String, `type`: Option[DataType]) extends DataExpr

final case class LambdaExpr[T <: PhraseType](param: IdentifierExpr, body: Expr[T]) extends Expr[ExpType ->T]

final case class ApplyExpr[T <: PhraseType](fun: Expr[ExpType -> T], arg: DataExpr) extends Expr[T]

final case class NatDependentLambdaExpr[T <: PhraseType](x: NatIdentifier, body: Expr[T]) extends Expr[`(nat)->`[T]]

final case class NatDependentApplyExpr[T <: PhraseType](fun: Expr[`(nat)->`[T]], arg: Nat) extends Expr[T]

final case class TypeDependentLambdaExpr[T <: PhraseType](x: DataTypeIdentifier, body: Expr[T]) extends Expr[`(dt)->`[T]]

final case class TypeDependentApplyExpr[T <: PhraseType](fun: Expr[`(dt)->`[T]], arg: DataType) extends Expr[T]

final case class IfThenElseExpr[T <: PhraseType](cond: DataExpr, thenE: Expr[T], elseE: Expr[T]) extends Expr[T]

final case class UnaryOpExpr(op: UnaryOp.Op.Value, e: DataExpr) extends DataExpr

final case class BinOpExpr(op: BinOp.Op.Value, lhs: DataExpr, rhs: DataExpr) extends DataExpr

final case class LiteralExpr(d: OperationalSemantics.Data, dt: DataType) extends DataExpr

trait PrimitiveExpr extends DataExpr {
  def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType]

  def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr
}

object Expr {
  // substitutes `phrase` for `for` in `in`, i.e. in [ phrase / for ]
  def substitute[T1 <: PhraseType, T2 <: PhraseType](expr: Expr[T1],
                                                     `for`: Expr[T1],
                                                     in: Expr[T2]): Expr[T2] = {
    object Visitor extends VisitAndRebuild.Visitor {
      override def apply[T <: PhraseType](e: Expr[T]): Result[Expr[T]] = {
        if (`for` == e) {
          Stop(expr.asInstanceOf[Expr[T]])
        } else {
          Continue(e, this)
        }
      }
    }

    VisitAndRebuild(in, Visitor)
  }
}