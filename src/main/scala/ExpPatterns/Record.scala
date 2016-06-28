package ExpPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType.->
import opencl.generator.OpenCLAST.Expression

import scala.xml.Elem

case class Record(fst: Phrase[ExpType],
                  snd: Phrase[ExpType])
  extends ExpPattern with GeneratableExpPattern {

  override def typeCheck(): ExpType = {
    ExpType(RecordType( TypeChecker(fst).dataType, TypeChecker(snd).dataType ))
  }

  override def eval(s: Store): Data = {
    RecordData(
      OperationalSemantics.eval(s, fst),
      OperationalSemantics.eval(s, snd))
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[ExpType] = {
    Record(VisitAndRebuild(fst, f), VisitAndRebuild(snd, f))
  }

  override def toOpenCL(ocl: ToOpenCL): Expression = ???

  override def prettyPrint: String = s"(${PrettyPrinter(fst)}, ${PrettyPrinter(snd)})"

  override def xmlPrinter: Elem =
    <record>
      <fst>
        {Core.xmlPrinter(fst)}
      </fst>
      <snd>
        {Core.xmlPrinter(snd)}
      </snd>
    </record>

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???
}
