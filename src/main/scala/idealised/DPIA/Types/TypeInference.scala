package idealised.DPIA.Types

import idealised.DPIA.{Phrases, _}
import idealised.DPIA.Phrases._
import idealised.SurfaceLanguage._

class TypeInferenceException(msg: String) extends TypeException("Failed to infer types:\n" + msg)

object ExpressionToPhrase {

  def error(found: String, expected: String): Nothing = {
    throw new TypeInferenceException(s"Found $found but expected $expected")
  }

  def error(msg: String): Nothing = {
    throw new TypeInferenceException(msg)
  }

  type SubstitutionMap = scala.collection.Map[Expr[_], Phrase[_]]

  def apply[T <: PhraseType](e: Expr[T], subs: SubstitutionMap): Phrase[T] = {
    inferType(e, subs)
  }

  private def inferType[T <: PhraseType](e: Expr[T],
                                         subs: SubstitutionMap): Phrase[T] = {
    e match {
      case i@IdentifierExpr(name, t) =>
        if (subs.contains(i)) {
          subs(i).asInstanceOf[Phrase[T]]
        } else {
          t match {
            case None => error(s"Found Identifier $name without type")
            case Some(dt) => Phrases.Identifier(name, ExpType(dt))
          }
        }

      case LambdaExpr(param, body) =>
        inferType(param, subs) match {
          case newParam: Identifier[ExpType] =>
            Lambda(newParam, inferType(body, subs))
          case _ => throw new Exception
        }

      case ApplyExpr(fun, arg) => Apply(inferType(fun, subs), inferType(arg, subs))

      case NatDependentLambdaExpr(x, e) =>
        NatDependentLambda(x, inferType(e, subs))

      case NatDependentApplyExpr(f, x) =>
        NatDependentApply(inferType(f, subs), x)

      case TypeDependentLambdaExpr(x, e) =>
        TypeDependentLambda(x, inferType(e, subs))

      case TypeDependentApplyExpr(f, x) =>
        TypeDependentApply(inferType(f, subs), x)

      case IfThenElseExpr(cond, thenE, elseE) =>
        IfThenElse(inferType(cond, subs), inferType(thenE, subs), inferType(elseE, subs))

      case UnaryOpExpr(op, e) => UnaryOp(op, inferType(e, subs))

      case BinOpExpr(op, lhs, rhs) => BinOp(op, inferType(lhs, subs), inferType(rhs, subs))

      case LiteralExpr(d, dt) => Literal(d, dt)

      case p: PrimitiveExpr => p.inferTypes(subs)
    }
  }

  def setParamAndInferType[T <: PhraseType](f: Expr[ExpType -> T],
                                            t: ExpType,
                                            subs: SubstitutionMap): Phrase[ExpType -> T] = {
    f match {
      case LambdaExpr(x, e) =>
        val newX = Identifier[ExpType](newName(), t)
        val newE = apply(e, subs.updated(x, newX))
        Lambda(newX, newE)
      case _ => throw new Exception("This should not happen")
    }
  }

  def setParamsAndInferTypes[T <: PhraseType](f: Expr[ExpType -> (ExpType -> T)],
                                              t1: ExpType,
                                              t2: ExpType,
                                              subs: SubstitutionMap): Phrase[ExpType -> (ExpType -> T)] = {
    f match {
      case LambdaExpr(x, e1) =>
        val newX = Identifier[ExpType](newName(), t1)
        e1 match {
          case LambdaExpr(y, e2) =>
            val newY = Identifier[ExpType](newName(), t2)
            val newE2 = apply(e2, subs.updated(x, newX).updated(y, newY))
            Lambda(newX, Lambda(newY, newE2))
          case _ => throw new Exception("This should not happen")
        }
      case _ => throw new Exception("This should not happen")
    }
  }
}