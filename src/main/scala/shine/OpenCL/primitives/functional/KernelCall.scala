package shine.OpenCL.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.DSL._
import shine.DPIA.Semantics.OperationalSemantics
import shine.OpenCL.{GlobalSize, LocalSize}
import shine.OpenCL.primitives.imperative.KernelCallCmd

case class KernelCall(name: String,
                      localSize: LocalSize,
                      globalSize: GlobalSize,
                      inTs: Seq[DataType],
                      outT: DataType,
                      args: Seq[Phrase[ExpType]]) extends ExpPrimitive {
  (inTs zip args).foreach{
    case (inT, arg) => arg :: expT(inT, read)
  }
  override val t: ExpType = expT(outT, write)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] =
    KernelCall(name, localSize.visitAndRebuild(f), globalSize.visitAndRebuild(f),
      inTs.map(f.data), f.data(outT), args.map(VisitAndRebuild(_, f)))

  override def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = ???
  override def prettyPrint: String = ???
  override def xmlPrinter: xml.Elem = ???

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    def recurse(ts: Seq[Phrase[ExpType]],
                es: Seq[Phrase[ExpType]]): Phrase[CommType] = {
      ts match {
        case Nil =>
          KernelCallCmd(name, localSize, globalSize, A, es)
        case Seq(arg, tail@_*) =>
          con(arg)(λ(expT(arg.t.dataType, read))(e => recurse(tail, es :+ e)))
      }
    }

    recurse(args, Seq())
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???
}
