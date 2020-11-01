package shine.DPIA.FunctionalPrimitives

import rise.core.{exprs => lc}
import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.Assign
import shine.DPIA.Phrases.VisitAndRebuild.Visitor
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

object ForeignFunction {
  val Declaration: lc.ForeignFunction.Decl.type = lc.ForeignFunction.Decl
  val Definition: lc.ForeignFunction.Def.type = lc.ForeignFunction.Def
  type Declaration = lc.ForeignFunction.Decl
  type Definition = lc.ForeignFunction.Def
}

final case class ForeignFunction(funDecl: ForeignFunction.Declaration,
                                 inTs: Seq[DataType],
                                 outT: DataType,
                                 args: Seq[Phrase[ExpType]])
  extends ExpPrimitive {

  (inTs zip args).foreach {
    case (inT, arg) => arg :: expT(inT, read)
  }
  override val t: ExpType = expT(outT, read)

  override def eval(s: Store): Data = ???

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    ???
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    def recurse(ts: Seq[(Phrase[ExpType], DataType)],
                exps: Seq[Phrase[ExpType]],
                inTs: Seq[DataType]): Phrase[CommType] = {
      ts match {
        // with only one argument left to process return the assignment of the function call
        case Seq( (arg, inT) ) =>
          con(arg)(λ(expT(inT, read))(e =>
            outT match {
              // TODO: this is an ugly fix to avoid calling the function multiple times
              //  for pair assignment, see:
              // https://github.com/rise-lang/shine/issues/58
              case PairType(_, _) =>
                  val backendNew: (DataType, Phrase[VarType] => Phrase[CommType])
                    => Phrase[CommType] =
                  context match {
                    case _: shine.OpenCL.TranslationContext =>
                      shine.OpenCL.DSL.`new`(AddressSpace.Private) _
                    case _ =>
                      `new`.apply _
                  }
                  backendNew(outT, tmp =>
                    Assign(outT, tmp.wr, ForeignFunction(funDecl, inTs :+ inT, outT, exps :+ e)) `;`
                      C(tmp.rd)
                  )
              case _ => C( ForeignFunction(funDecl, inTs :+ inT, outT, exps :+ e) )
            }))
        // with a `tail` of arguments left, recurse
        case Seq( (arg, inT), tail@_* ) =>
          con(arg)(λ(expT(inT, read))(e => recurse(tail, exps :+ e, inTs :+ inT) ))
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
    ForeignFunction(funDecl, inTs.map(f.data), f.data(outT), args.map(VisitAndRebuild(_, f)))
  }
}
