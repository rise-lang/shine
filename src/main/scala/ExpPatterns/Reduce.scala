package ExpPatterns

import CommandPatterns.{ReduceIAcc, ReduceIExp}
import Core._
import Core.PhraseType._
import Core.OperationalSemantics._
import DSL._
import Rewriting.RewriteToImperative

abstract class AbstractReduce(f: Phrase[ExpType -> (ExpType -> ExpType)],
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

}

case class Reduce(f: Phrase[ExpType -> (ExpType -> ExpType)],
                  init: Phrase[ExpType],
                  array: Phrase[ExpType]) extends AbstractReduce(f, init, array) {

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    Reduce(
      OperationalSemantics.substitute(phrase, `for`, f),
      OperationalSemantics.substitute(phrase, `for`, init),
      OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def toC = ???

  override def prettyPrint: String = s"(reduce ${PrettyPrinter(f)} ${PrettyPrinter(init)} ${PrettyPrinter(array)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    RewriteToImperative.exp(array, λ(array.t) { x =>
      RewriteToImperative.exp(init, λ(init.t) { y =>
        ReduceIAcc(A,
          λ(A.t) { o => λ(array.t) { x => λ(init.t) { y => RewriteToImperative.acc(f(x)(y), o) } } },
          y,
          x
        )
      })
    })
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    RewriteToImperative.exp(array, λ(array.t) { x =>
      RewriteToImperative.exp(init, λ(init.t) { y =>
        ReduceIExp(C,
          λ(AccType(init.t.dataType)) { o => λ(array.t) { x => λ(init.t) { y => RewriteToImperative.acc(f(x)(y), o) } } },
          y,
          x
        )
      })
    })
  }
}
