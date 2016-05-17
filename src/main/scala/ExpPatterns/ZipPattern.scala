package ExpPatterns

import Core._
import Core.OperationalSemantics._

case class ZipPattern(lhs: Phrase[ExpType], rhs: Phrase[ExpType]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    (TypeChecker(lhs), TypeChecker(rhs)) match {
      case (ExpType(ArrayType(n, dt1)), ExpType(ArrayType(m, dt2))) if n == m =>
        ExpType(ArrayType(n, RecordType(dt1, dt2)))
      case t => error(t.toString(), "PairOfArrayTypes")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    ZipPattern(OperationalSemantics.substitute(phrase, `for`, lhs), OperationalSemantics.substitute(phrase, `for`, rhs))
  }

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, lhs), OperationalSemantics.eval(s, rhs)) match {
      case (ArrayData(lhsE), ArrayData(rhsE)) =>
        ArrayData((lhsE zip rhsE) map { p =>
          RecordData(p._1, p._2)
        })

      case _ => throw new Exception("This should not happen")
    }
  }

}