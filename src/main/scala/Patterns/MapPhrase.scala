package Patterns

import Core._
import Core.PhraseType._
import Core.OperationalSemantics._

case class MapPhrase(f: Phrase[ExpType -> ExpType], in: Phrase[ExpType]) extends Pattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(in) match {
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

  override def substitute[T <: PhraseType](p1: Phrase[T], p2: Phrase[T]): Pattern = {
    MapPhrase(OperationalSemantics.substitute(p1, p2, f), OperationalSemantics.substitute(p1, p2, in))
  }

  override def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = {
    import OperationalSemantics.implicits._
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, in) match {
      case ArrayData(xs) =>
        ArrayData(xs.map { x => OperationalSemantics.eval(s, fE(Literal(x))) })
    }
  }

}