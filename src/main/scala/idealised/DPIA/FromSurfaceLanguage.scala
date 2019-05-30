package idealised.DPIA

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.SurfaceLanguage.Types.Type
import idealised.SurfaceLanguage.VisitAndRebuild.Continue
import idealised.SurfaceLanguage._

object FromSurfaceLanguage {

  def apply(expr:Expr):Phrase[_ <: PhraseType] = {
    toPhrase(expr)
  }

  def toPhrase(expr:Expr):Phrase[_ <: PhraseType] = {
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

      case ln@LetNat(_, definition, _, _) =>
        val fv = freeVariables(definition)
        val scoped = scopeLetNat(fv, ln)

        Phrases.LetNat(
          scoped.binder, toPhrase(scoped.definition).asInstanceOf[Phrase[ExpType]], toPhrase(scoped.body)
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

  private def scopeLetNat(freeVariables:List[IdentifierExpr], inner:LetNat):LetNat = {
    assert(inner.t.isDefined)
    freeVariables match {
      case Nil => inner
      case freeV::rest =>
        val freshBinder = LetNatIdentifier()

        val rebuiltBody = VisitAndRebuild(inner.body, new VisitAndRebuild.Visitor {
          override def apply(ae: Nat): Nat = ae match {
            case NatFunCall(fun, args) if fun == inner.binder =>
              NatFunCall(fun, Seq(LetNatIdArg(freshBinder)) ++ args)
            case other => other
          }

          override def apply[T <: Type](t: T): T = ???
        })

        val rebuiltInner = inner.copy(
          definition = lambdaAbstraction(freeV, inner.definition),
          body = rebuiltBody
        )

        LetNat(
          binder = freshBinder,
          definition = freeV,
          body = rebuiltInner,
          t = rebuiltInner.t
        )
    }
  }

  private def lambdaAbstraction(v:IdentifierExpr, e:Expr):LambdaExpr = {
    assert(v.t.isDefined)
    assert(e.t.isDefined)
    LambdaExpr(v, e, Some(idealised.SurfaceLanguage.Types.FunctionType(v.t.get, e.t.get)))
  }

  private def freeVariables(expr: Expr):List[IdentifierExpr] = {
    var useSet:Set[IdentifierExpr] = Set()
    var defSet:Set[IdentifierExpr] = Set()

    object visitor extends VisitAndRebuild.Visitor {
      override def apply(e: Expr): VisitAndRebuild.Result[Expr] = {
        e match {
          case id:IdentifierExpr =>
            useSet += id
            Continue(id, this)
          case LambdaExpr(param, body, _) =>
            defSet += param
            Continue(body, this)
          case other => Continue(other, this)
        }
      }
    }

    VisitAndRebuild(expr, visitor)
    useSet.diff(defSet).toList
  }


}
