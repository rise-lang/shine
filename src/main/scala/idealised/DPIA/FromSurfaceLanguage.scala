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

      case LambdaExpr(param, body) =>
        Phrases.Lambda(
          apply(param).asInstanceOf[Phrases.Identifier[PhraseType]],
          apply(body))

      case ApplyExpr(fun, arg) =>
        Phrases.Apply(
          apply(fun).asInstanceOf[Phrase[FunctionType[ExpType, PhraseType]]],
          apply(arg).asInstanceOf[Phrase[ExpType]])

      case NatDependentLambdaExpr(x, body) =>
        Phrases.NatDependentLambda(x, apply(body))

      case NatDependentApplyExpr(fun, arg) =>
        Phrases.NatDependentApply(
          apply(fun).asInstanceOf[Phrases.Phrase[Types.NatDependentFunctionType[Types.PhraseType]]],
          arg)

      case TypeDependentLambdaExpr(x, body) =>
        Phrases.TypeDependentLambda(Types.DataTypeIdentifier(x.name), apply(body))

      case TypeDependentApplyExpr(fun, arg) =>
        Phrases.TypeDependentApply(
          apply(fun).asInstanceOf[Phrases.Phrase[Types.TypeDependentFunctionType[Types.PhraseType]]],
          arg)

      case IfThenElseExpr(cond, thenE, elseE) =>
        Phrases.IfThenElse(apply(cond).asInstanceOf[Phrases.Phrase[Types.ExpType]],
          apply(thenE).asInstanceOf[Phrases.Phrase[Types.PhraseType]],
          apply(elseE).asInstanceOf[Phrases.Phrase[Types.PhraseType]])

      case UnaryOpExpr(op, e) =>
        Phrases.UnaryOp(op, apply(e).asInstanceOf[Phrases.Phrase[Types.ExpType]])

      case BinOpExpr(op, lhs, rhs) =>
        Phrases.BinOp(op,
          apply(lhs).asInstanceOf[Phrases.Phrase[Types.ExpType]],
          apply(rhs).asInstanceOf[Phrases.Phrase[Types.ExpType]])

      case LiteralExpr(d) =>
        Phrases.Literal(Semantics.OperationalSemantics.Data(d))

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
