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

  override def substitute[T <: PhraseType](p1: Phrase[T], p2: Phrase[T]): Pattern = {
    ZipPhrase(OperationalSemantics.substitute(p1, p2, lhs), OperationalSemantics.substitute(p1, p2, rhs))
  }

  override def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = {
    import OperationalSemantics.implicits._
    (OperationalSemantics.eval(s, lhs), OperationalSemantics.eval(s, rhs)) match {
      case (ArrayData(lhsE), ArrayData(rhsE)) =>
        ArrayData((lhsE zip rhsE) map { p =>
          RecordData(p._1, p._2)
        })
    }
  }

}