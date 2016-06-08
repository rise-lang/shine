package AccPatterns

import Core._
import Core.OperationalSemantics._
import opencl.generator.OpenCLAST.VarRef

case class IdxAcc(array: Phrase[AccType], index: Phrase[ExpType]) extends AccPattern {

  override def typeCheck(): AccType = {
    import TypeChecker._
    check(TypeChecker(index), ExpType(int))
    TypeChecker(array) match {
      case AccType(ArrayType(n, t)) => AccType(t)
      case t => error(t.toString, "ArrayType")
    }
  }

  override def eval(s: Store): AccIdentifier = {
    val arrayE = OperationalSemantics.eval(s, array)
    val indexE = OperationalSemantics.eval(s, index) match {
      case IntData(i) => i
      case _ => throw new Exception("This should not happen")
    }
    ArrayAccessIdentifier(arrayE, indexE)
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): AccPattern = {
    IdxAcc(
      OperationalSemantics.substitute(phrase, `for`, array),
      OperationalSemantics.substitute(phrase, `for`, index))
  }

  override def toC = Printer.toC(array) + "[" + Printer.toC(index) + "]"

  override def toOpenCL: VarRef = {
    VarRef(ToOpenCL.acc(array).name, null, ToOpenCL.exp(index))
  }

  override def prettyPrint: String = s"${PrettyPrinter(array)}[${PrettyPrinter(index)}]"

}
