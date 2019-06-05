package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.VisitAndRebuild.Visitor
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics.{Data, Store}
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.language.reflectiveCalls
import scala.xml.Elem


final case class OpenCLFunction(name: String,
                                inTs: Seq[DataType],
                                outT: DataType,
                                args: Seq[Phrase[ExpType]])
  extends ExpPrimitive {

  override val t: ExpType =
    (inTs zip args).foreach{
      case (inT, arg) => arg :: exp"[$inT]"
    } -> exp"[$outT]"

  override def visitAndRebuild(f: Visitor): Phrase[ExpType] = {
    OpenCLFunction(name, inTs.map(f(_)), f(outT), args.map(VisitAndRebuild(_, f)))
  }

//  override def codeGenExp(env: Environment): Expression = {
//    FunctionCall(name, args.map(OpenCLOldCodeGenerator.exp(_, env)).toList)
//  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = s"$name(${args.map(PrettyPhrasePrinter(_)).mkString(",")})"

  override def xmlPrinter: Elem =
    <OpenCLFunction name={ToString(name)} inTs={ToString(inTs)} outT={ToString(outT)}>
      {args.map(Phrases.xmlPrinter(_))}
    </OpenCLFunction>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    def recurse(ts: Seq[(Phrase[ExpType], DataType)],
                exps: Seq[Phrase[ExpType]],
                inTs: Seq[DataType]): Phrase[CommType] = {
      ts match {
        // with only one argument left to process return the assignment of the OpenCLFunction call
        case Seq( (arg, inT) ) =>
          con(arg)(λ(exp"[$inT]")(e =>
            A :=|outT| OpenCLFunction(name, inTs :+ inT, outT, exps :+ e) ))
        // with a `tail` of arguments left, recurse
        case Seq( (arg, inT), tail@_* ) =>
          con(arg)(λ(exp"[$inT]")(e => recurse(tail, exps :+ e, inTs :+ inT) ))
      }
    }

    recurse(args zip inTs, Seq(), Seq())
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def continuationTranslation(C: Phrase[->[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    def recurse(ts: Seq[(Phrase[ExpType], DataType)],
                es: Seq[Phrase[ExpType]],
                inTs: Seq[DataType]): Phrase[CommType] = {
      ts match {
        // with only one argument left to process continue with the OpenCLFunction call
        case Seq( (arg, inT) ) =>
          con(arg)(λ(exp"[$inT]")(e => C(OpenCLFunction(name, inTs :+ inT, outT, es :+ e)) ))
        // with a `tail` of arguments left, recurse
        case Seq( (arg, inT), tail@_* ) =>
          con(arg)(λ(exp"[$inT]")(e => recurse(tail, es :+ e, inTs :+ inT) ))
      }
    }

    recurse(args zip inTs, Seq(), Seq())
  }

}

