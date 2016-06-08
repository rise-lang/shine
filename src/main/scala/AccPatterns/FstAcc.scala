package AccPatterns

import Core._
import Core.OperationalSemantics._
import opencl.generator.OpenCLAST.OclAstNode

case class FstAcc(record: Phrase[AccType]) extends AccPattern {

  override def typeCheck(): AccType = {
    import TypeChecker._
    TypeChecker(record) match {
      case AccType(RecordType(fst, snd)) => AccType(fst)
      case t => error(t.toString, "Something else")
    }
  }

  override def eval(s: Store): AccIdentifier = {
    OperationalSemantics.eval(s, record) match {
      case r: RecordIdentiers => r.fst
      case _ => throw new Exception("This should not happen")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): AccPattern = {
    FstAcc(OperationalSemantics.substitute(phrase, `for`, record))
  }

  override def toC = ???

  override def toOpenCL: OclAstNode = ???

  override def prettyPrint: String = s"${PrettyPrinter(record)}._1"
}
