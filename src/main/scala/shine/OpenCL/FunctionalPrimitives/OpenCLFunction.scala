package shine.OpenCL.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases.VisitAndRebuild.Visitor
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types._
import shine.DPIA._

import scala.language.reflectiveCalls
import scala.xml.Elem


final case class OpenCLFunction(name: String,
                                inTs: Seq[DataType],
                                outT: DataType,
                                args: Seq[Phrase[ExpType]])
  extends ExpPrimitive {

  (inTs zip args).foreach{
    case (inT, arg) => arg :: expT(inT, read)
  }
  override val t: ExpType = expT(outT, read)

  override def visitAndRebuild(f: Visitor): Phrase[ExpType] = {
    OpenCLFunction(name, inTs.map(f.data), f.data(outT), args.map(VisitAndRebuild(_, f)))
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
          con(arg)(λ(expT(inT, read))(e =>
            A :=|outT| OpenCLFunction(name, inTs :+ inT, outT, exps :+ e) ))
        // with a `tail` of arguments left, recurse
        case Seq( (arg, inT), tail@_* ) =>
          con(arg)(λ(expT(inT, read))(e => recurse(tail, exps :+ e, inTs :+ inT) ))
      }
    }

    recurse(args zip inTs, Seq(), Seq())
  }

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    def recurse(ts: Seq[(Phrase[ExpType], DataType)],
                es: Seq[Phrase[ExpType]],
                inTs: Seq[DataType]): Phrase[CommType] = {
      ts match {
        // with only one argument left to process continue with the OpenCLFunction call
        case Seq( (arg, inT) ) =>
          con(arg)(λ(expT(inT, read))(e => C(OpenCLFunction(name, inTs :+ inT, outT, es :+ e)) ))
        // with a `tail` of arguments left, recurse
        case Seq( (arg, inT), tail@_* ) =>
          con(arg)(λ(expT(inT, read))(e => recurse(tail, es :+ e, inTs :+ inT) ))
      }
    }

    recurse(args zip inTs, Seq(), Seq())
  }

}

