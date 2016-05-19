package ExpPatterns

import Core._
import Core.PhraseType._
import Core.OperationalSemantics._

case class Reduce(f: Phrase[ExpType -> (ExpType -> ExpType)],
                  init: Phrase[ExpType],
                  array: Phrase[ExpType]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    (TypeChecker(init), TypeChecker(array)) match {
      case (ExpType(dt2), ExpType(ArrayType(n, dt1))) =>
        setParamType(f, ExpType(dt1))
        setSecondParamType(f, ExpType(dt2))
        TypeChecker(f) match {
          case FunctionType(ExpType(t1), FunctionType(ExpType(t2), ExpType(t3))) =>
            if (dt1 == t1 && dt2 == t2 && dt2 == t3) ExpType(dt2)
            else {
              error(dt1.toString + ", " + t1.toString + " as well as " +
                dt2.toString + ", " + t2.toString + " and " + t3.toString,
                expected = "them to match")
            }
          case t => error(t.toString, "FunctionType")
        }
      case t => error(t.toString, "(ExpType, ArrayType)")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    Reduce(
      OperationalSemantics.substitute(phrase, `for`, f),
      OperationalSemantics.substitute(phrase, `for`, init),
      OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def eval(s: Store): Data = {
    val fE = OperationalSemantics.eval(s, f)(BinaryFunctionEvaluator)
    val initE = OperationalSemantics.eval(s, init)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(Vector(xs.fold(initE) {
          (x, y) => OperationalSemantics.eval(s, fE(LiteralPhrase(x))(LiteralPhrase(y)))
        }))
      case _ => throw new Exception("This should not happen")
    }
  }

  override def toC = ???

  override def prettyPrint: String = s"(reduce ${PrettyPrinter(f)} ${PrettyPrinter(init)} ${PrettyPrinter(array)})"

}

// Implementation with PairType instead of curried FunctionType to represent binary function
//case class ReducePhrase(f: Phrase[ExpType x ExpType -> ExpType],
//                        init: Phrase[ExpType],
//                        array: Phrase[ExpType]) extends ExpPattern {
//
//  override def typeCheck(): ExpType = {
//    import TypeChecker._
//    (TypeChecker(init), TypeChecker(array)) match {
//      case (ExpType(dt2), ExpType(ArrayType(n, dt1))) =>
//        setParamType(f, PairType(ExpType(dt1), ExpType(dt2)))
//        TypeChecker(f) match {
//          case FunctionType(PairType(ExpType(t1), ExpType(t2)), ExpType(t3)) =>
//            if (dt1 == t1 && dt2 == t2 && dt2 == t3) ExpType(dt2)
//            else {
//              error(dt1.toString + ", " + t1.toString + " as well as " +
//                dt2.toString + ", " + t2.toString + " and " + t3.toString,
//                expected = "them to match")
//            }
//          case t => error(t.toString, "FunctionType")
//        }
//      case t => error(t.toString, "ArrayType")
//    }
//  }
//
//  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
//    ReducePhrase(
//      OperationalSemantics.substitute(phrase, `for`, f),
//      OperationalSemantics.substitute(phrase, `for`, init),
//      OperationalSemantics.substitute(phrase, `for`, array))
//  }
//
//  override def eval(s: Store): Data = {
//    val fE = OperationalSemantics.eval(s, f)
//    val initE = OperationalSemantics.eval(s, init)
//    OperationalSemantics.eval(s, array) match {
//      case ArrayData(xs) =>
//        ArrayData(Vector(xs.fold(initE) {
//          (x, y) => OperationalSemantics.eval(s, fE(Pair(Literal(x), Literal(y))))
//        }))
//      case _ => throw new Exception("This should not happen")
//    }
//  }
//
//}
