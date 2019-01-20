package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.RewriteToImperative
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.VisitAndRebuild.Visitor
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics.{Data, Store}
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.language.reflectiveCalls
import scala.xml.Elem

object ForeignFunction {
  final case class Declaration(name: String, argNames: Seq[String], body: String)
}

final case class ForeignFunction(funDecl: ForeignFunction.Declaration,
                                 inTs: Seq[DataType],
                                 outT: DataType,
                                 args: Seq[Phrase[ExpType]])
  extends ExpPrimitive {

  override lazy val `type`: ExpType =
    (inTs zip args).foreach {
      case (inT, arg) => arg :: exp"[$inT]"
    } -> exp"[$outT]"

  override def eval(s: Store): Data = ???

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._

    def recurse(ts: Seq[(Phrase[ExpType], DataType)],
                exps: Seq[Phrase[ExpType]],
                inTs: Seq[DataType]): Phrase[CommandType] = {
      ts match {
        // with only one argument left to process return the assignment of the OpenCLFunction call
        case Seq((arg, inT)) =>
          con(arg)(λ(exp"[$inT]")(e =>
            A :=| outT | ForeignFunction(funDecl, inTs :+ inT, outT, exps :+ e)))
        // with a `tail` of arguments left, recurse
        case Seq((arg, inT), tail@_*) =>
          con(arg)(λ(exp"[$inT]")(e => recurse(tail, exps :+ e, inTs :+ inT)))
      }
    }

    recurse(args zip inTs, Seq(), Seq())
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    import RewriteToImperative._

    def recurse(ts: Seq[(Phrase[ExpType], DataType)],
                exps: Seq[Phrase[ExpType]],
                inTs: Seq[DataType]): Phrase[CommandType] = {
      ts match {
        // with only one argument left to process return the assignment of the OpenCLFunction call
        case Seq( (arg, inT) ) =>
          con(arg)(λ(exp"[$inT]")(e =>
            C( ForeignFunction(funDecl, inTs :+ inT, outT, exps :+ e) )) )
        // with a `tail` of arguments left, recurse
        case Seq( (arg, inT), tail@_* ) =>
          con(arg)(λ(exp"[$inT]")(e => recurse(tail, exps :+ e, inTs :+ inT) ))
      }
    }

    recurse(args zip inTs, Seq(), Seq())
  }

  override def prettyPrint: String = s"${funDecl.name}(${args.map(PrettyPhrasePrinter(_)).mkString(",")})"

  override def xmlPrinter: Elem =
    <ForeignFunction name={ToString(funDecl.name)} inTs={ToString(inTs)} outT={ToString(outT)}>
      {args.map(Phrases.xmlPrinter(_))}
    </ForeignFunction>

  override def visitAndRebuild(f: Visitor): Phrase[ExpType] = {
    ForeignFunction(funDecl, inTs.map(f(_)), f(outT), args.map(VisitAndRebuild(_, f)))
  }
}
