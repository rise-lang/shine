package Core

class TypeException(msg: String) extends Exception(msg)

object TypeChecker {

  def apply[T <: PhraseType](phrase: Phrase[T]): Unit = {
    phrase match {
      case IdentPhrase(_, t) =>
        if (t == null) { error("null", expected = "a valid type") }

      case LambdaPhrase(x, p) => x.typeCheck(); p.typeCheck()

      case ApplyPhrase(p, q) =>
        p.typeCheck()
        q.typeCheck()
        p.t match {
          case FunctionType(t1, t2) =>
            check(t1, q.t)
          case x => error(x.toString, FunctionType.toString)
        }

      case NatDependentLambdaPhrase(_, p) => p.typeCheck()

      case NatDependentApplyPhrase(p, _) => p.typeCheck()

      case PairPhrase(p, q) => p.typeCheck(); q.typeCheck()

      case Proj1Phrase(p) => p.typeCheck()

      case Proj2Phrase(p) => p.typeCheck()

      case IfThenElsePhrase(cond, thenP, elseP) =>
        cond.typeCheck()
        thenP.typeCheck()
        elseP.typeCheck()
        check(cond.t, exp"[$int]" | exp"[$bool]")
        check(thenP.t, elseP.t)

      case LiteralPhrase(_, _) =>

      case UnaryOpPhrase(op, x) =>
        x.typeCheck()
        x.t match {
          case ExpType(_) =>
          case y => error(y.toString, ExpType.toString)
        }

      case BinOpPhrase(op, lhs, rhs) =>
        lhs.typeCheck()
        rhs.typeCheck()
        (lhs.t, rhs.t) match {
          case (ExpType(dt1), ExpType(dt2))
            if dt1.isInstanceOf[BasicType] && dt2.isInstanceOf[BasicType] =>
              if (lhs.t != rhs.t) {
                error(s"${lhs.t} and ${rhs.t}", expected = "them to match")
              }
          case (x1, x2) =>
            error(s"$x1 $op $x2", s"exp[b] $op exp[b]")
        }

      case c: Combinator[_] => c.typeCheck()
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

  def check(found: PhraseType, test: (PhraseType => Unit)): Unit = {
    test(found)
  }

  implicit class InferenceHelper[T <: PhraseType](p: Phrase[T]) {
    def checkType(pt: PhraseType): Unit = {
      p.typeCheck()
      check(p.t, pt)
    }
  }

  implicit class ReverseInferenceHelper(pt: PhraseType) {
    def ::[T <: PhraseType](p: Phrase[T]) = p checkType pt
    def `:`[T <: PhraseType](p: Phrase[T]) = p checkType pt
  }

  implicit class CheckHelper(p1: PhraseType) {
    def |(p2: PhraseType): (PhraseType => Unit) = (p: PhraseType) => {
      if (!(p == p1 || p == p2)) {
        error(ToString(p), expected = ToString(p1) + " or " + ToString(p2))
      }
    }
  }

}
