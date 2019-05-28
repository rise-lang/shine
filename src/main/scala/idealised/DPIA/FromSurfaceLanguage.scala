package idealised.DPIA

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.SurfaceLanguage._

object FromSurfaceLanguage {

  def apply(expr:Expr):Phrase[_ <: PhraseType] = {
    toPhrase(convertSurfaceLangaugeInNats(expr))
  }

  def toPhrase(expr: Expr): Phrase[_ <: PhraseType] = {
    assert(expr.t.isDefined) // e must have all types inferred
    expr match {
      case IdentifierExpr(name, t) => t match {
        case None => throw new Exception(s"Found Identifier without a type")
        case Some(dt) =>
          Phrases.Identifier(name, Types.ExpType(Types.DataType(dt)))
      }

      case LambdaExpr(param, body, _) =>
        Phrases.Lambda(
          toPhrase(param).asInstanceOf[Phrases.Identifier[PhraseType]],
          toPhrase(body))

      case ApplyExpr(fun, arg, _) =>
        Phrases.Apply(
          toPhrase(fun).asInstanceOf[Phrase[FunctionType[ExpType, PhraseType]]],
          toPhrase(arg).asInstanceOf[Phrase[ExpType]])

      case NatDependentLambdaExpr(x, body, _) =>
        Phrases.NatDependentLambda(x, toPhrase(body))

      case NatDependentApplyExpr(fun, arg, _) =>
        Phrases.NatDependentApply(
          toPhrase(fun).asInstanceOf[Phrases.Phrase[Types.NatDependentFunctionType[Types.PhraseType]]],
          arg)

      case TypeDependentLambdaExpr(x, body, _) =>
        Phrases.TypeDependentLambda(Types.DataTypeIdentifier(x.name), toPhrase(body))

      case TypeDependentApplyExpr(fun, arg, _) =>
        Phrases.TypeDependentApply(
          toPhrase(fun).asInstanceOf[Phrases.Phrase[Types.TypeDependentFunctionType[Types.PhraseType]]],
          arg)

      case LetNat(fun, definition, body, _) =>
        Phrases.LetNat(
          fun, toPhrase(definition).asInstanceOf[Phrase[ExpType]], toPhrase(body)
        )

      case IfThenElseExpr(cond, thenE, elseE, _) =>
        Phrases.IfThenElse(toPhrase(cond).asInstanceOf[Phrases.Phrase[Types.ExpType]],
          toPhrase(thenE).asInstanceOf[Phrases.Phrase[Types.PhraseType]],
          toPhrase(elseE).asInstanceOf[Phrases.Phrase[Types.PhraseType]])

      case UnaryOpExpr(op, e, _) =>
        Phrases.UnaryOp(op, toPhrase(e).asInstanceOf[Phrases.Phrase[Types.ExpType]])

      case BinOpExpr(op, lhs, rhs, _) =>
        Phrases.BinOp(op,
          toPhrase(lhs).asInstanceOf[Phrases.Phrase[Types.ExpType]],
          toPhrase(rhs).asInstanceOf[Phrases.Phrase[Types.ExpType]])

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
    toPhrase(expr).asInstanceOf[Phrase[T]]

  private def convertSurfaceLangaugeInNats(expr: Expr):Expr = {
    val fun:NatFunArg => NatFunArg = {
      case SurfaceExpArg(arg) => DPIAExpArg(FromSurfaceLanguage(arg).asInstanceOf[Phrase[ExpType]])
      case other => other
    }

    VisitAndRebuild(expr, new VisitAndRebuild.Visitor {
      override def apply(ae: Nat): Nat =
        ae.mapNatFunArg({
          case SurfaceExpArg(arg) => DPIAExpArg(FromSurfaceLanguage(arg).asInstanceOf[Phrase[ExpType]])
          case other => other
        })
    })
  }
}
