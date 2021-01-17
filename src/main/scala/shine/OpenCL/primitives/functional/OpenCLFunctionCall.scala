package shine.OpenCL.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

import scala.language.reflectiveCalls

@expPrimitive
final case class OpenCLFunctionCall(name: String,
                                    inTs: Seq[DataType],
                                    outT: DataType,
                                    args: Seq[Phrase[ExpType]]
                                   ) extends ExpPrimitive with ContinuationTranslatable with AcceptorTranslatable {
  (inTs zip args).foreach{
    case (inT, arg) => arg :: expT(inT, read)
  }
  override val t: ExpType = expT(outT, read)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] = {
    def recurse(ts: Seq[(Phrase[ExpType], DataType)],
                exps: Seq[Phrase[ExpType]],
                inTs: Seq[DataType]): Phrase[CommType] = {
      ts match {
        // with only one argument left to process return the assignment of the OpenCLFunction call
        case Seq( (arg, inT) ) =>
          con(arg)(λ(expT(inT, read))(e =>
            A :=|outT| OpenCLFunctionCall(name, inTs :+ inT, outT, exps :+ e) ))
        // with a `tail` of arguments left, recurse
        case Seq( (arg, inT), tail@_* ) =>
          con(arg)(λ(expT(inT, read))(e => recurse(tail, exps :+ e, inTs :+ inT) ))
      }
    }

    recurse(args zip inTs, Seq(), Seq())
  }

  def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                             (implicit context: TranslationContext): Phrase[CommType] = {
    def recurse(ts: Seq[(Phrase[ExpType], DataType)],
                es: Seq[Phrase[ExpType]],
                inTs: Seq[DataType]): Phrase[CommType] = {
      ts match {
        // with only one argument left to process continue with the OpenCLFunction call
        case Seq( (arg, inT) ) =>
          con(arg)(λ(expT(inT, read))(e =>
            C(OpenCLFunctionCall(name, inTs :+ inT, outT, es :+ e)) ))
        // with a `tail` of arguments left, recurse
        case Seq( (arg, inT), tail@_* ) =>
          con(arg)(λ(expT(inT, read))(e => recurse(tail, es :+ e, inTs :+ inT) ))
      }
    }

    recurse(args zip inTs, Seq(), Seq())
  }
}
