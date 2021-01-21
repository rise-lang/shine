package shine.OpenCL.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Semantics.OperationalSemantics
import shine.OpenCL.{GlobalSize, LocalSize}

final case class Run(localSize: LocalSize,
                     globalSize: GlobalSize,
                     dt: DataType,
                     input: Phrase[ExpType]
                    ) extends ExpPrimitive {
  input :: expT(dt, write)
  override val t: ExpType = expT(dt, write)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] =
    Run(localSize.visitAndRebuild(f), globalSize.visitAndRebuild(f),
      f.data(dt), VisitAndRebuild(input, f))

  override def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = ???
  override def prettyPrint: String = ???
  override def xmlPrinter: xml.Elem = ???

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???
}
