package ExpPatterns

import AccPatterns.JoinAcc
import Core.OperationalSemantics._
import Core.PhraseType.->
import Core._
import Rewriting.RewriteToImperative
import DSL._

case class Join(array: Phrase[ExpType]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(n, ArrayType(m, dt))) =>
        ExpType(ArrayType(n*m, dt))
      case x => error(x.toString, "ArrayType(ArrayType)")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    Join(OperationalSemantics.substitute(phrase, `for`, array))
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

  override def toC = ???

  override def prettyPrint: String = s"(join ${PrettyPrinter(array)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    RewriteToImperative.acc(array, JoinAcc(A))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    RewriteToImperative.exp(array, λ(array.t) { x =>
      C(Join(x))
    })
  }
}