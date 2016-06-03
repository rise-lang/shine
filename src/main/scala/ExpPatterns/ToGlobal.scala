package ExpPatterns

import AccPatterns.ToGlobalAcc
import Core.OperationalSemantics.{Data, Store}
import Core.PhraseType.->
import Core._
import Rewriting.RewriteToImperative
import DSL._

case class ToGlobal(p: Phrase[ExpType]) extends ExpPattern {
  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(p) match {
      case ExpType(dt) => ExpType(dt)
      case x => error(x.toString, "ExpType")
    }
  }

  override def eval(s: Store): Data = OperationalSemantics.eval(s, p)

  override def toC: String = ???

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    ToGlobal(OperationalSemantics.substitute(phrase, `for`, p))
  }

  override def prettyPrint: String = s"(toGlobal ${PrettyPrinter(p)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    RewriteToImperative.acc(p, ToGlobalAcc(A))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    RewriteToImperative.exp(p, λ(p.t) { x =>
      C(ToGlobal(x))
    })
  }
}
