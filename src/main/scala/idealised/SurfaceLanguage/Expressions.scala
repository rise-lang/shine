package idealised.SurfaceLanguage

import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.SurfaceLanguage.DSL.DataExpr

sealed trait Expr[T <: PhraseType]

final case class IdentifierExpr(name: String, `type`: Option[DataType]) extends DataExpr

final case class LambdaExpr[T <: PhraseType](param: IdentifierExpr, body: Expr[T]) extends Expr[ExpType ->T]

final case class ApplyExpr[T <: PhraseType](fun: Expr[ExpType -> T], arg: DataExpr) extends Expr[T]

final case class NatDependentLambdaExpr[T <: PhraseType](x: NatIdentifier, body: Expr[T]) extends Expr[`(nat)->`[T]]

final case class NatDependentApplyExpr[T <: PhraseType](fun: Expr[`(nat)->`[T]], arg: Nat) extends Expr[T]

final case class TypeDependentLambdaExpr[T <: PhraseType](x: DataTypeIdentifier, body: Expr[T]) extends Expr[`(dt)->`[T]]

final case class TypeDependentApplyExpr[T <: PhraseType](fun: Expr[`(dt)->`[T]], arg: DataType) extends Expr[T]

final case class IfThenElseExpr[T <: PhraseType](cond: DataExpr, thenE: Expr[T], elseE: Expr[T]) extends Expr[T]

final case class UnaryOpExpr(op: Operators.Unary.Value, e: DataExpr) extends DataExpr

final case class BinOpExpr(op: Operators.Binary.Value, lhs: DataExpr, rhs: DataExpr) extends DataExpr

final case class LiteralExpr(d: OperationalSemantics.Data, dt: DataType) extends DataExpr

trait PrimitiveExpr extends DataExpr {
  def inferTypes(subs: TypeInference.SubstitutionMap): Phrases.Primitive[ExpType]

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

object Operators {
  object Unary extends Enumeration {
    val NEG = Value("-")
  }

  object Binary extends Enumeration {
    val ADD = Value("+")
    val SUB = Value("-")
    val MUL = Value("*")
    val DIV = Value("/")
    val MOD = Value("%")
    val GT = Value(">")
    val LT = Value("<")
  }
}