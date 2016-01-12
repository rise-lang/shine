package Patterns

import Core._
import Core.PhraseType._
import Core.OperationalSemantics._

case class IteratePhrase(n: Int, f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) extends Pattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case t@ExpType(ArrayType(m, dt)) =>
        setParamType(f, t)
        TypeChecker(f) match {
          case FunctionType(t1, t2) if (t1 == t2) && (t == t1) =>
            t // improve and capture effect on array size
          case ft => error(ft.toString, "FunctionType")
        }
      case t => error(t.toString, "ArrayType")
    }
  }

  override def substitute[T <: PhraseType](p1: Phrase[T], p2: Phrase[T]): Pattern = {
    IteratePhrase(n, OperationalSemantics.substitute(p1, p2, f), OperationalSemantics.substitute(p1, p2, array))
  }

  override def eval(s: Store): Data = {
    import OperationalSemantics.implicits._
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        var a = array
        for (_ <- 0 until n) {
          a = fE(a)
        }
        OperationalSemantics.eval(s, a)
    }
  }
}