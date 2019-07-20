package idealised.DPIA

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.SurfaceLanguage._

object FromSurfaceLanguage {

  def apply(expr: Expr): Phrase[_ <: PhraseType] = {
    assert(expr.t.isDefined) // e must have all types inferred
    expr match {
      case IdentifierExpr(name, t) => t match {
        case None => throw new Exception(s"Found Identifier without a type")
        case Some(dt) =>
          Phrases.Identifier(name, Types.ExpType(Types.DataType(dt)))
      }

      case LambdaExpr(param, body, _) =>
        Phrases.Lambda(
          apply(param).asInstanceOf[Phrases.Identifier[PhraseType]],
          apply(body))

      case ApplyExpr(fun, arg, _) =>
        Phrases.Apply(
          apply(fun).asInstanceOf[Phrase[FunType[ExpType, PhraseType]]],
          apply(arg).asInstanceOf[Phrase[ExpType]])

      case NatDependentLambdaExpr(x, body, _) =>
        Phrases.DepLambda[NatKind](NatIdentifier(x.name, x.range))(apply(body))

      case NatDependentApplyExpr(fun, arg, _) =>
        Phrases.DepApply[NatKind, Types.PhraseType](
          apply(fun).asInstanceOf[Phrases.Phrase[Types.DepFunType[NatKind, Types.PhraseType]]],
          arg)

      case TypeDependentLambdaExpr(x, body, _) =>
        Phrases.DepLambda[DataKind](Types.DataTypeIdentifier(x.name))(apply(body))

      case TypeDependentApplyExpr(fun, arg, _) =>
        Phrases.DepApply[DataKind, Types.PhraseType](
          apply(fun).asInstanceOf[Phrases.Phrase[Types.DepFunType[Types.DataKind, Types.PhraseType]]],
          arg)

      case LetNat(binder, definition, body, _) =>
        Phrases.LetNat(
          binder, apply(definition).asInstanceOf[Phrase[ExpType]], apply(body)
        )

      case IfThenElseExpr(cond, thenE, elseE, _) =>
        Phrases.IfThenElse(apply(cond).asInstanceOf[Phrases.Phrase[Types.ExpType]],
          apply(thenE).asInstanceOf[Phrases.Phrase[Types.PhraseType]],
          apply(elseE).asInstanceOf[Phrases.Phrase[Types.PhraseType]])

      case UnaryOpExpr(op, e, _) =>
        Phrases.UnaryOp(op, apply(e).asInstanceOf[Phrases.Phrase[Types.ExpType]])

      case BinOpExpr(op, lhs, rhs, _) =>
        Phrases.BinOp(op,
          apply(lhs).asInstanceOf[Phrases.Phrase[Types.ExpType]],
          apply(rhs).asInstanceOf[Phrases.Phrase[Types.ExpType]])

      case LiteralExpr(d, _) =>
        Phrases.Literal(Semantics.OperationalSemantics.Data(d))

      case NatExpr(n) => Phrases.Natural(n)

      case p: PrimitiveExpr =>
        FromSurfaceLanguagePrimitives(p) match {
          case Some(e) => e
          case None => throw new Exception(s"Don't know how to translate $p into DPIA")
        }
    }
  }

  def asPhrase[T <: PhraseType](expr: Expr): Phrase[T] =
    apply(expr).asInstanceOf[Phrase[T]]

}
