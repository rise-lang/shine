package AccPatterns

import Core._
import Core.OperationalSemantics._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

case class SplitAcc(array: Phrase[AccType]) extends AccPattern {

  override def typeCheck(): AccType = {
    import TypeChecker._
    TypeChecker(array) match {
      case AccType(ArrayType(n, ArrayType(m, dt))) =>
        AccType(ArrayType(n*m, dt))
      case x => error(x.toString, "ArrayType(ArrayType)")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): AccPattern = {
    SplitAcc(OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def eval(s: Store): AccIdentifier = {
    ???
  }

  override def toC = ???

  override def toOpenCL: VarRef = ???

  def toOpenCL(arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef = ???

  override def prettyPrint: String = s"(split ${PrettyPrinter(array)})"

}
