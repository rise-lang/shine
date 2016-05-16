package ExpPatterns

import Core.OperationalSemantics._
import Core._

case class JoinPhrase(array: Phrase[ExpType]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(n, ArrayType(m, dt))) =>
        ExpType(ArrayType(n*m, dt))
      case t => error(t.toString, "ArrayType(ArrayType)")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    JoinPhrase(OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, array) match {
      case ArrayData(outer) =>
        val arrays = outer.map {
          case ArrayData(inner) => inner
          case _ => throw new Exception("This should not happen")
        }
        ArrayData(arrays.flatten)

      case _ => throw new Exception("This should not happen")
    }
  }

}