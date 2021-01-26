package shine.OpenCL.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Semantics.OperationalSemantics
import shine.OpenCL.{GlobalSize, LocalSize}

case class KernelCallCmd(name: String,
                         localSize: LocalSize,
                         globalSize: GlobalSize,
                         output: Phrase[AccType],
                         args: Seq[Phrase[ExpType]]) extends CommandPrimitive {
  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommType] =
    KernelCallCmd(name, localSize.visitAndRebuild(f), globalSize.visitAndRebuild(f),
      VisitAndRebuild(output, f), args.map(VisitAndRebuild(_, f)))

  override def eval(s: OperationalSemantics.Store): OperationalSemantics.Store = ???
  override def prettyPrint: String = ???
  override def xmlPrinter: xml.Elem = ???
}

