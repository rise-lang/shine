package CommandPatterns

import Core._
import Core.PhraseType._
import Core.OperationalSemantics._

import DSL._

import AccPatterns._
import ExpPatterns._

abstract class AbstractMapI(out: Phrase[AccType],
                            f: Phrase[AccType -> (ExpType -> CommandType)],
                            in: Phrase[ExpType]) extends CommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    (TypeChecker(out), TypeChecker(in)) match {
      case (AccType(ArrayType(n, dt2)), ExpType(ArrayType(m, dt1))) if n == m =>
        setParamType(f, AccType(dt2))
        setSecondParamType(f, ExpType(dt1))
        TypeChecker(f) match {
          case FunctionType(AccType(t1), FunctionType(ExpType(t2), CommandType())) =>
            if (dt2 == t1 && dt1 == t2) CommandType()
            else {
              error(dt2.toString + " and " + t1.toString +
                ", " + dt1.toString + " and " + t2.toString,
                expected = "them to match")
            }
          case x => error(x.toString, "FunctionType")
        }
      case x => error(x.toString, "(ArrayType, ArrayType)")
    }
  }

  override def eval(s: Store): Store = {
    val fE = OperationalSemantics.eval(s, f)(BinaryFunctionEvaluator)
    val n = TypeChecker(in) match {
      case ExpType(ArrayType(len, _)) => len
    }

    (0 until n).foldLeft(s)((sOld, i) => {
      val comm = fE(IdxAcc(out, LiteralPhrase(i)))(Idx(in, LiteralPhrase(i)))
      OperationalSemantics.eval(sOld, comm)
    })
  }

  override def toC = ???

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): CommandPattern = {
    makeMapI(
      OperationalSemantics.substitute(phrase, `for`, out),
      OperationalSemantics.substitute(phrase, `for`, f),
      OperationalSemantics.substitute(phrase, `for`, in))
  }

  def makeMapI: (Phrase[AccType], Phrase[AccType -> (ExpType -> CommandType)], Phrase[ExpType]) => AbstractMapI

  override def prettyPrint: String = s"${this.getClass.getSimpleName} ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(in)}"

}

case class MapI(out: Phrase[AccType],
                f: Phrase[AccType -> (ExpType -> CommandType)],
                in: Phrase[ExpType]) extends AbstractMapI(out, f, in) {

  override def makeMapI = MapI

  override def substituteImpl: Phrase[CommandType] = {
    `parFor`(length(in), out, i => o => {
      f(o)(in `@` i)
    })
  }

}
