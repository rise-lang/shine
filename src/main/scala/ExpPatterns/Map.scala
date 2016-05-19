package ExpPatterns

import Core._
import Core.PhraseType._
import Core.OperationalSemantics._

case class Map(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(n, dt1)) =>
        setParamType(f, ExpType(dt1))
        TypeChecker(f) match {
          case FunctionType(ExpType(t), ExpType(dt2)) =>
            if (dt1 == t) ExpType(ArrayType(n, dt2))
            else {
              error(dt1.toString + " and " + t.toString, expected = "them to match")
            }
          case t => error(t.toString, "FunctionType")
        }
      case t => error(t.toString, "ArrayType")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    Map(OperationalSemantics.substitute(phrase, `for`, f), OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def eval(s: Store): Data = {
    import OperationalSemantics._
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(xs.map { x => OperationalSemantics.eval(s, fE(LiteralPhrase(x))) })

      case _ => throw new Exception("This should not happen")
    }
  }

  override def toC = ???

  override def prettyPrint: String = s"(map ${PrettyPrinter(f)} ${PrettyPrinter(array)})"

}