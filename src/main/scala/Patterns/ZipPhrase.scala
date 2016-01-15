package Patterns

import Core._
import Core.OperationalSemantics._

case class ZipPhrase(lhs: Phrase[ExpType], rhs: Phrase[ExpType]) extends Pattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    (TypeChecker(lhs), TypeChecker(rhs)) match {
      case (ExpType(ArrayType(n, a)), ExpType(ArrayType(m, b))) if n == m =>
        ExpType(ArrayType(n, RecordType(a, b)))
      case t => error(t.toString(), "PairOfArrayTypes")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): Pattern = {
    ZipPhrase(OperationalSemantics.substitute(phrase, `for`, lhs), OperationalSemantics.substitute(phrase, `for`, rhs))
  }

  override def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = {
    (OperationalSemantics.eval(s, lhs), OperationalSemantics.eval(s, rhs)) match {
      case (ArrayData(lhsE), ArrayData(rhsE)) =>
        ArrayData((lhsE zip rhsE) map { p =>
          RecordData(p._1, p._2)
        })

      case _ => throw new Exception("This should not happen")
    }
  }

}