package shine.OpenCL.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.DSL._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.primitives.functional.{ForeignFunction, ForeignFunctionCall}

case class KernelCall(name: String,
                      inTs: Seq[DataType],
                      outT: DataType,
                      args: Seq[Phrase[ExpType]]) extends ExpPrimitive {
  (inTs zip args).foreach{
    case (inT, arg) => arg :: expT(inT, read)
  }
  override val t: ExpType = expT(outT, read)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] =
    KernelCall(name, inTs.map(f.data), f.data(outT), args.map(VisitAndRebuild(_, f)))

  override def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = ???
  override def prettyPrint: String = ???
  override def xmlPrinter: xml.Elem = ???

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    val decl = ForeignFunction.Declaration(name, None)
    def recurse(ts: Seq[(Phrase[ExpType], DataType)],
                es: Seq[Phrase[ExpType]],
                inTs: Seq[DataType]): Phrase[CommType] = {
      ts match {
        case Nil =>
          // FIXME: should not use an ForeignFunctionCall, for prototyping only
          C(ForeignFunctionCall(decl, inTs, outT, es))
        case Seq( (arg, inT), tail@_* ) =>
          con(arg)(λ(expT(inT, read))(e => recurse(tail, es :+ e, inTs :+ inT) ))
      }
    }

    recurse(args zip inTs, Seq(), Seq())
  }
}
