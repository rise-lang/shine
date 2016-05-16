package ExpPatterns

import Core._
import Core.PhraseType._
import Core.OperationalSemantics._

case class ReducePhrase(f: Phrase[ExpType x ExpType -> ExpType], init: Phrase[ExpType], array: Phrase[ExpType]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(n, dt)) =>
        setParamType(f, PairType(ExpType(dt), ExpType(dt)))
        TypeChecker(f) match {
          case FunctionType(PairType(ExpType(t1), ExpType(t2)), ExpType(t3)) =>
            if (dt == t1 && dt == t2 && dt == t3) ExpType(dt)
            else {
              error(dt.toString + ", " + t1.toString + ", " + t2.toString +
                " and " + t3.toString,
                expected = "them to match")
            }
          case t => error(t.toString, "FunctionType")
        }
      case t => error(t.toString, "ArrayType")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    ReducePhrase(
      OperationalSemantics.substitute(phrase, `for`, f),
      OperationalSemantics.substitute(phrase, `for`, init),
      OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def eval(s: Store): Data = {
    val fE = OperationalSemantics.eval(s, f)
    val initE = OperationalSemantics.eval(s, init)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(Vector(xs.fold(initE) {
          (x, y) => OperationalSemantics.eval(s, fE(Pair(Literal(x), Literal(y))))
        }))
      case _ => throw new Exception("This should not happen")
    }
  }

}