package idealised.SurfaceLanguage

import idealised.DPIA
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.ExpType
import idealised.DPIA.{Types => DPIAType}
import idealised.SurfaceLanguage.{Types => SurfaceType}
import shapeless.Lazy

object ToDPIA {
  def apply[T <: SurfaceType.Type,
            PT <: DPIAType.PhraseType](expr: Expr[T])
                                      (implicit to: ToPhraseType[T, PT]): Phrase[PT] = {
    to.toPhrase(expr)
  }

  def toExpr(expr: Expr[SurfaceType.DataType]): Phrase[DPIAType.ExpType] = {
    ???
  }

  def toExprFunExpr(expr: Expr[SurfaceType.DataType -> SurfaceType.DataType]): Phrase[DPIAType.FunctionType[DPIAType.ExpType, DPIAType.ExpType]] = {
    ???
  }
}

trait ToPhraseType[T <: SurfaceType.Type, PT <: DPIAType.PhraseType] {
  def toPhrase(expr: Expr[T]): Phrase[PT]
}

object ToPhraseType {
  implicit val dataTypeConversion: ToPhraseType[SurfaceType.DataType, DPIAType.ExpType] =
    new ToPhraseType[SurfaceType.DataType, DPIAType.ExpType] {

      override def toPhrase(expr: Expr[SurfaceType.DataType]): Phrase[DPIAType.ExpType] = {
        expr match {
          case i: IdentifierExpr => i.`type` match {
            case Some(dt) => DPIA.Phrases.Identifier(i.name, DPIAType.ExpType(DPIAType.DataType(dt)))
            case None => throw new Exception("")
          }

          case ApplyExpr(fun, arg) =>
            DPIA.Phrases.Apply(
              ToDPIA[SurfaceType.DataType -> SurfaceType.DataType,
                     DPIAType.FunctionType[DPIAType.ExpType, DPIAType.ExpType]](fun),
              toPhrase(arg))

          case IfThenElseExpr(cond, thenE, elseE) =>
            DPIA.Phrases.IfThenElse[DPIAType.ExpType](toPhrase(cond), toPhrase(thenE), toPhrase(elseE))

          case UnaryOpExpr(op, e) =>
            DPIA.Phrases.UnaryOp(op, toPhrase(e))

          case BinOpExpr(op, lhs, rhs) =>
            DPIA.Phrases.BinOp(op, toPhrase(lhs), toPhrase(rhs))

          case LiteralExpr(d, dt) =>
            DPIA.Phrases.Literal(DPIA.Semantics.OperationalSemantics.Data(d), DPIAType.DataType(dt))

          case p: PrimitiveExpr => p.toDPIA
        }
      }
    }

  implicit val funConv1: ToPhraseType[
      SurfaceType.FunctionType[SurfaceType.DataType, SurfaceType.DataType],
      DPIA.Types.FunctionType[DPIAType.ExpType, DPIAType.ExpType]
    ] =
      new ToPhraseType[
          SurfaceType.FunctionType[SurfaceType.DataType, SurfaceType.DataType],
          DPIA.Types.FunctionType[DPIAType.ExpType, DPIAType.ExpType]
        ] {
          override def toPhrase(expr: Expr[SurfaceType.DataType -> SurfaceType.DataType]):
          Phrase[DPIAType.FunctionType[DPIAType.ExpType, DPIAType.ExpType]] = {
            expr match {
              case LambdaExpr(param, body) => ToDPIA(param) match {
                  case newP: DPIA.Phrases.Identifier[DPIAType.ExpType] =>
                    DPIA.Phrases.Lambda(newP, ToDPIA(body))
                  case _ => throw new Exception("")
                }
              case _ => ???
            }
          }
        }

  implicit def funConv2[T <: SurfaceType.Type, PT <: DPIAType.PhraseType]
    (implicit evidence: Lazy[ToPhraseType[T, PT]]):
      ToPhraseType[SurfaceType.FunctionType[SurfaceType.DataType, T],
                   DPIAType.FunctionType[DPIAType.ExpType, PT]] =
    new ToPhraseType[
        SurfaceType.FunctionType[SurfaceType.DataType, T],
        DPIAType.FunctionType[DPIAType.ExpType, PT]
      ] {
        override def toPhrase(expr: Expr[SurfaceType.DataType -> T]): Phrase[DPIAType.FunctionType[DPIAType.ExpType, PT]] = {
          expr match {
            case LambdaExpr(param, body) => ToDPIA(param) match {
              case newP: DPIA.Phrases.Identifier[DPIAType.ExpType] =>
                DPIA.Phrases.Lambda(newP, ToDPIA(body)(evidence.value))
              case _ => throw new Exception("")
            }
            case _ => ???
          }
        }
      }

//  implicit def typeDepFunTypeConversion[T <: SurfaceType.Type](
//                                                                implicit tConversion: Lazy[ToPhraseType[T]]
//                                                              ): ToPhraseType[`(dt)->`[T]] =
//    new ToPhraseType[`(dt)->`[T]] {
//      override type PT = DPIA.Types.TypeDependentFunctionType[tConversion.value.PT]
//
//      override def toPhrase(expr: Expr[`(dt)->`[T]]):
//        Phrase[DPIAType.TypeDependentFunctionType[tConversion.value.PT]] = {
//        ???
//      }
//    }
//
  implicit def natDepFunTypeConversion[T <: SurfaceType.Type, PT <: DPIAType.PhraseType]
    (implicit evidence: Lazy[ToPhraseType[T, PT]]):
      ToPhraseType[SurfaceType.NatDependentFunctionType[T], DPIAType.NatDependentFunctionType[PT]] =
    new ToPhraseType[SurfaceType.NatDependentFunctionType[T], DPIAType.NatDependentFunctionType[PT]] {
      override def toPhrase(expr: Expr[`(nat)->`[T]]):
        Phrase[DPIAType.NatDependentFunctionType[PT]] = {
          ???
      }
    }
}

