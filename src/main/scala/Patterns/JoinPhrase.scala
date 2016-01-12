package Patterns

import Core.OperationalSemantics._
import Core._

case class JoinPhrase(array: Phrase[ExpType]) extends Pattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(n, ArrayType(m, t))) =>
        ExpType(ArrayType(n*m, t))
      case t => error(t.toString, "ArrayType(ArrayType)")
    }
  }

  override def substitute[T <: PhraseType](p1: Phrase[T], p2: Phrase[T]): Pattern = {
    JoinPhrase(OperationalSemantics.substitute(p1, p2, array))
  }

  override def eval(s: Store): Data = {
    import OperationalSemantics.implicits._
    OperationalSemantics.eval(s, array) match {
      case ArrayData(outer) =>
        val arrays = outer.map(row => row match {
          case ArrayData(inner) => inner
        })
        ArrayData(arrays.flatten)
    }
  }

}