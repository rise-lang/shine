package ExpPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType.->
import opencl.generator.OpenCLAST.{VarRef, Expression, Literal}

case class Idx(array: Phrase[ExpType], index: Phrase[ExpType]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    check(TypeChecker(index), ExpType(int))
    TypeChecker(array) match {
      case ExpType(ArrayType(_, dt)) => ExpType(dt)
      case x => error(x.toString, "ArrayType")
    }
  }

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, array), OperationalSemantics.eval(s, index)) match {
      case (ArrayData(xs), IntData(i)) => xs(i)
      case _ => throw new Exception("This should not happen")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    Idx(
      OperationalSemantics.substitute(phrase, `for`, array),
      OperationalSemantics.substitute(phrase, `for`, index))
  }

  override def toC = Printer.toC(array) + "[" + Printer.toC(index) + "]"

  override def toOpenCL: Expression = {
    val v = ToOpenCL.exp(array) match {
      case VarRef(name, _, _) => name
      case _ => throw new Exception("This should not happen")
    }
    VarRef(v, null, ToOpenCL.exp(index))
  }

  override def prettyPrint: String = s"(${PrettyPrinter(array)})[${PrettyPrinter(index)}]"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???
}
