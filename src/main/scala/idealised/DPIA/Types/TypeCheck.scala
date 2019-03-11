package idealised.DPIA.Types

import idealised.DPIA.Phrases._
import idealised.DPIA._

class TypeException(msg: String) extends Exception(msg)

object TypeCheck {

  def apply[T <: PhraseType](phrase: Phrase[T]): Unit = {
    phrase match {
      case Identifier(_, _) =>

      case Lambda(x, p) => TypeCheck(x); TypeCheck(p)

      case Apply(p, q) =>
        TypeCheck(p)
        TypeCheck(q)
        p.t match {
          case FunctionType(t1, _) =>
            check(t1, q.t)
          case x => error(x.toString, FunctionType.toString)
        }

      case NatDependentLambda(_, p) => TypeCheck(p)

      case TypeDependentLambda(_, p) => TypeCheck(p)

      case NatDependentApply(p, _) => TypeCheck(p)

      case TypeDependentApply(p, _) => TypeCheck(p)

      case Pair(p, q) => TypeCheck(p); TypeCheck(q)

      case Proj1(p) => TypeCheck(p)

      case Proj2(p) => TypeCheck(p)

      case IfThenElse(cond, thenP, elseP) =>
        TypeCheck(cond)
        TypeCheck(thenP)
        TypeCheck(elseP)
        check(cond.t, exp"[$int]" | exp"[$bool]")
        check(thenP.t, elseP.t)

      case Literal(_) =>

      case Natural(_) =>

      case UnaryOp(op, x) =>
        TypeCheck(x)
        x.t match {
          case ExpType(_) =>
          case y => error(y.toString, s"$op ${ExpType.toString}")
        }

      case BinOp(op, lhs, rhs) =>
        TypeCheck(lhs)
        TypeCheck(rhs)
        (lhs.t, rhs.t) match {
          case (ExpType(dt1), ExpType(dt2))
            if dt1.isInstanceOf[BasicType] && dt2.isInstanceOf[BasicType] =>
              if (lhs.t != rhs.t) {
                error(s"${lhs.t} and ${rhs.t}", expected = "them to match")
              }
          case (x1, x2) =>
            error(s"$x1 $op $x2", s"exp[b] $op exp[b]")
        }

      case c: Primitive[_] => c.`type`
    }
  }

  def error(found: String, expected: String): Nothing = {
    throw new TypeException(s"Type error: found $found expected $expected")
  }

  def error(found: PhraseType, expected: PhraseType): Nothing = {
    error(ToString(found), ToString(expected))
  }

  def check(found: PhraseType, expected: PhraseType): Unit = {
    if (found != expected) {
      error(found, expected)
    }
  }

  def check(found: PhraseType, test: PhraseType => Unit): Unit = {
    test(found)
  }

  implicit class InferenceHelper[T <: PhraseType](p: Phrase[T]) {
    def checkType(pt: PhraseType): Unit = {
      TypeCheck(p)
      check(p.t, pt)
    }
  }
}
