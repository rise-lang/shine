//package idealised.SurfaceLanguage
//
//import idealised.DPIA
//import idealised.DPIA.Phrases.Phrase
//import idealised.DPIA.{Types => DPIAType}
//import idealised.SurfaceLanguage.{Types => SurfaceType}
//import shapeless.Lazy
//
//object ToDPIA {
//  def apply[T <: SurfaceType.Type,
//            PT <: DPIAType.PhraseType](expr: Expr[T])
//                                      (implicit to: ToPhraseType[T, PT]): Phrase[PT] = {
//    to.toPhrase(expr)
//  }
//}
//
//trait ToPhraseType[T <: SurfaceType.Type, PT <: DPIAType.PhraseType] {
//  def toPhrase(expr: Expr[T]): Phrase[PT]
//}
//
//object ToPhraseType {
//  implicit val baseCases: ToPhraseType[SurfaceType.DataType, DPIAType.ExpType] =
//    new ToPhraseType[SurfaceType.DataType, DPIAType.ExpType] {
//
//      override def toPhrase(expr: Expr[SurfaceType.DataType]): Phrase[DPIAType.ExpType] = {
//        expr match {
//          case i: IdentifierExpr => i.t match {
//            case Some(dt) => DPIA.Phrases.Identifier(i.name, DPIAType.ExpType(DPIAType.DataType(dt)))
//            case None => throw new Exception("")
//          }
//
//          case ApplyExpr(fun, arg) =>
//            DPIA.Phrases.Apply(
//              ToDPIA[SurfaceType.DataType -> SurfaceType.DataType,
//                     DPIAType.FunctionType[DPIAType.ExpType, DPIAType.ExpType]](fun),
//              ToDPIA(arg))
//
//          case NatDependentApplyExpr(fun, arg) =>
//            DPIA.Phrases.NatDependentApply(ToDPIA(fun), arg)
//
//          case TypeDependentApplyExpr(fun, arg) =>
//            DPIA.Phrases.TypeDependentApply(ToDPIA(fun), arg)
//
//          case IfThenElseExpr(cond, thenE, elseE) =>
//            DPIA.Phrases.IfThenElse(toPhrase(cond), toPhrase(thenE), toPhrase(elseE))
//
//          case UnaryOpExpr(op, e) =>
//            DPIA.Phrases.UnaryOp(op, toPhrase(e))
//
//          case BinOpExpr(op, lhs, rhs) =>
//            DPIA.Phrases.BinOp(op, toPhrase(lhs), toPhrase(rhs))
//
//          case LiteralExpr(d) =>
//            DPIA.Phrases.Literal(DPIA.Semantics.OperationalSemantics.Data(d)) // , DPIAType.DataType(d.dataType)
//
//          case p: PrimitiveExpr => p.toPhrase
//        }
//      }
//    }
//
//  implicit val fun1: ToPhraseType[
//      SurfaceType.FunctionType[SurfaceType.DataType, SurfaceType.DataType],
//      DPIA.Types.FunctionType[DPIAType.ExpType, DPIAType.ExpType]
//    ] =
//      new ToPhraseType[
//          SurfaceType.FunctionType[SurfaceType.DataType, SurfaceType.DataType],
//          DPIA.Types.FunctionType[DPIAType.ExpType, DPIAType.ExpType]
//        ] {
//          override def toPhrase(expr: Expr[SurfaceType.DataType -> SurfaceType.DataType]):
//          Phrase[DPIAType.FunctionType[DPIAType.ExpType, DPIAType.ExpType]] = {
//            expr match {
//              case LambdaExpr(param, body) =>
//                ToDPIA(param) match {
//                  case newP: DPIA.Phrases.Identifier[DPIAType.ExpType] =>
//                    DPIA.Phrases.Lambda(newP, ToDPIA(body))
//                  case _ => throw new Exception("")
//                }
//
//              case ApplyExpr(fun, arg) =>
//                DPIA.Phrases.Apply(
//                  ToDPIA[
//                    SurfaceType.FunctionType[SurfaceType.DataType,
//                      SurfaceType.FunctionType[SurfaceType.DataType, SurfaceType.DataType]],
//                    DPIA.Types.FunctionType[DPIAType.ExpType,
//                      DPIA.Types.FunctionType[DPIAType.ExpType, DPIAType.ExpType]]](fun),
//                  ToDPIA(arg))
//
//              case IfThenElseExpr(cond, thenE, elseE) =>
//                DPIA.Phrases.IfThenElse(ToDPIA(cond), ToDPIA(thenE), ToDPIA(elseE))
//
//              case NatDependentApplyExpr(fun, arg) =>
//                DPIA.Phrases.NatDependentApply(ToDPIA(fun), arg)
//
//              case TypeDependentApplyExpr(fun, arg) =>
//                DPIA.Phrases.TypeDependentApply(ToDPIA(fun), arg)
//            }
//          }
//        }
//
//  implicit def fun2[T <: SurfaceType.Type, PT <: DPIAType.PhraseType]
//    (implicit evidence: Lazy[ToPhraseType[T, PT]]):
//      ToPhraseType[SurfaceType.FunctionType[SurfaceType.DataType, T],
//                   DPIAType.FunctionType[DPIAType.ExpType, PT]
//        ] =
//    new ToPhraseType[
//        SurfaceType.FunctionType[SurfaceType.DataType, T],
//        DPIAType.FunctionType[DPIAType.ExpType, PT]
//      ] {
//        override def toPhrase(expr: Expr[SurfaceType.DataType -> T]): Phrase[DPIAType.FunctionType[DPIAType.ExpType, PT]] = {
//          expr match {
//            case LambdaExpr(param, body) =>
//              ToDPIA(param) match {
//                case newP: DPIA.Phrases.Identifier[DPIAType.ExpType] =>
//                  DPIA.Phrases.Lambda(newP, ToDPIA(body)(evidence.value))
//                case _ => throw new Exception("")
//              }
//
//            case ApplyExpr(fun, arg) =>
//              DPIA.Phrases.Apply(
//                ToDPIA[
//                  SurfaceType.FunctionType[SurfaceType.DataType,
//                    SurfaceType.FunctionType[SurfaceType.DataType, T]],
//                  DPIA.Types.FunctionType[DPIAType.ExpType,
//                    DPIA.Types.FunctionType[DPIAType.ExpType, PT]]](fun),
//                ToDPIA(arg))
//
//            case IfThenElseExpr(cond, thenE, elseE) =>
//              DPIA.Phrases.IfThenElse( // ([DPIAType.FunctionType[DPIAType.ExpType, PT]](
//                ToDPIA(cond),
//                ToDPIA[
//                  SurfaceType.FunctionType[SurfaceType.DataType, T],
//                  DPIAType.FunctionType[DPIAType.ExpType, PT]](thenE),
//                ToDPIA[
//                  SurfaceType.FunctionType[SurfaceType.DataType, T],
//                  DPIAType.FunctionType[DPIAType.ExpType, PT]](elseE))
//
//            case NatDependentApplyExpr(fun, arg) =>
//              DPIA.Phrases.NatDependentApply(ToDPIA(fun), arg)
//
//            case TypeDependentApplyExpr(fun, arg) =>
//              DPIA.Phrases.TypeDependentApply(ToDPIA(fun), arg)
//          }
//        }
//      }
//
//  implicit def typeDefFun[T <: SurfaceType.Type, PT <: DPIAType.PhraseType]
//    (implicit evidence: Lazy[ToPhraseType[T, PT]]):
//      ToPhraseType[SurfaceType.TypeDependentFunctionType[T], DPIAType.TypeDependentFunctionType[PT]] =
//    new ToPhraseType[SurfaceType.TypeDependentFunctionType[T], DPIAType.TypeDependentFunctionType[PT]] {
//      override def toPhrase(expr: Expr[`(dt)->`[T]]):
//      Phrase[DPIAType.TypeDependentFunctionType[PT]] = {
//        expr match {
//          case ApplyExpr(fun, arg) =>
//            DPIA.Phrases.Apply(
//              ToDPIA[SurfaceType.DataType -> SurfaceType.TypeDependentFunctionType[T],
//                DPIAType.FunctionType[DPIAType.ExpType, DPIAType.TypeDependentFunctionType[PT]]](fun),
//              baseCases.toPhrase(arg))
//
//          case IfThenElseExpr(cond, thenE, elseE) =>
//            DPIA.Phrases.IfThenElse( //[DPIAType.NatDependentFunctionType[PT]](
//              ToDPIA(cond),
//              ToDPIA[
//                SurfaceType.TypeDependentFunctionType[T], DPIAType.TypeDependentFunctionType[PT]](thenE),
//              ToDPIA[
//                SurfaceType.TypeDependentFunctionType[T], DPIAType.TypeDependentFunctionType[PT]](elseE))
//
//          case NatDependentApplyExpr(fun, arg) =>
//            DPIA.Phrases.NatDependentApply( //[DPIAType.NatDependentFunctionType[PT]](
//              ToDPIA(fun),
//              arg)
//
//          case TypeDependentLambdaExpr(x, body) =>
//            DPIA.Phrases.TypeDependentLambda(
//              DPIAType.DataTypeIdentifier(x.name),
//              ToDPIA(body)(evidence.value))
//
//          case TypeDependentApplyExpr(fun, arg) =>
//            DPIA.Phrases.TypeDependentApply(
//              ToDPIA[
//                SurfaceType.TypeDependentFunctionType[SurfaceType.TypeDependentFunctionType[T]],
//                DPIAType.TypeDependentFunctionType[DPIAType.TypeDependentFunctionType[PT]]
//                ](fun),
//              arg)
//        }
//      }
//    }
//
//  implicit def natDepFun[T <: SurfaceType.Type, PT <: DPIAType.PhraseType]
//    (implicit evidence: Lazy[ToPhraseType[T, PT]]):
//      ToPhraseType[SurfaceType.NatDependentFunctionType[T], DPIAType.NatDependentFunctionType[PT]] =
//    new ToPhraseType[SurfaceType.NatDependentFunctionType[T], DPIAType.NatDependentFunctionType[PT]] {
//      override def toPhrase(expr: Expr[`(nat)->`[T]]):
//        Phrase[DPIAType.NatDependentFunctionType[PT]] = {
//          expr match {
//            case ApplyExpr(fun, arg) =>
//              DPIA.Phrases.Apply(
//                ToDPIA[SurfaceType.DataType -> SurfaceType.NatDependentFunctionType[T],
//                       DPIAType.FunctionType[DPIAType.ExpType, DPIAType.NatDependentFunctionType[PT]]](fun),
//                baseCases.toPhrase(arg))
//
//            case IfThenElseExpr(cond, thenE, elseE) =>
//              DPIA.Phrases.IfThenElse( //[DPIAType.NatDependentFunctionType[PT]](
//                ToDPIA(cond),
//                ToDPIA[
//                  SurfaceType.NatDependentFunctionType[T], DPIAType.NatDependentFunctionType[PT]](thenE),
//                ToDPIA[
//                  SurfaceType.NatDependentFunctionType[T], DPIAType.NatDependentFunctionType[PT]](elseE))
//
//            case NatDependentLambdaExpr(x, body) =>
//              DPIA.Phrases.NatDependentLambda(x, ToDPIA(body)(evidence.value))
//
//            case NatDependentApplyExpr(fun, arg) =>
//              DPIA.Phrases.NatDependentApply( //[DPIAType.NatDependentFunctionType[PT]](
//                ToDPIA[
//                  SurfaceType.NatDependentFunctionType[SurfaceType.NatDependentFunctionType[T]],
//                  DPIAType.NatDependentFunctionType[DPIAType.NatDependentFunctionType[PT]]
//                  ](fun),
//                arg)
//
//            case TypeDependentApplyExpr(fun, arg) =>
//              DPIA.Phrases.TypeDependentApply(ToDPIA(fun), arg)
//          }
//      }
//    }
//}
//
