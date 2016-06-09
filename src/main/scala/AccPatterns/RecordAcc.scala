package AccPatterns

import Core._
import Core.OperationalSemantics._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

case class RecordAcc(fst: Phrase[AccType], snd: Phrase[AccType]) extends AccPattern {

  override def typeCheck(): AccType = {
    AccType(RecordType( TypeChecker(fst).dataType, TypeChecker(snd).dataType ))
  }

  override def eval(s: Store): AccIdentifier = {
    RecordIdentiers(
      OperationalSemantics.eval(s, fst),
      OperationalSemantics.eval(s, snd))
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): AccPattern = {
    RecordAcc(
      OperationalSemantics.substitute(phrase, `for`, fst),
      OperationalSemantics.substitute(phrase, `for`, snd))
  }

  override def toC = ???

  override def toOpenCL: VarRef = ???

  def toOpenCL(arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef = ???

  override def prettyPrint: String = s"(${PrettyPrinter(fst)}, ${PrettyPrinter(snd)})"

}
