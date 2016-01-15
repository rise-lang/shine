package Patterns

import Core._
import Core.PhraseType._
import Core.OperationalSemantics._

case class MapPhrase(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) extends Pattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(n, dt)) =>
        setParamType(f, ExpType(dt))
        TypeChecker(f) match {
          case FunctionType(ExpType(t1), ExpType(t2)) =>
            if (dt == t1) ExpType(ArrayType(n, t2))
            else {
              error(dt.toString + " and " + t1.toString,
                expected = "them to match")
            }
          case t => error(t.toString, "FunctionType")
        }
      case t => error(t.toString, "ArrayType")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): Pattern = {
    MapPhrase(OperationalSemantics.substitute(phrase, `for`, f), OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = {
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(xs.map { x => OperationalSemantics.eval(s, fE(Literal(x))) })

      case _ => throw new Exception("This should not happen")
    }
  }

}