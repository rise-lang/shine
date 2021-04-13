package shine.DPIA.primitives.functional

import rise.{core => lc}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

object ForeignFunction {
  val Declaration: lc.ForeignFunction.Decl.type = lc.ForeignFunction.Decl
  val Definition: lc.ForeignFunction.Def.type = lc.ForeignFunction.Def
  type Declaration = lc.ForeignFunction.Decl
  type Definition = lc.ForeignFunction.Def
}

@expPrimitive
final case class ForeignFunctionCall(funDecl: ForeignFunction.Declaration,
                                     inTs: Seq[DataType],
                                     outT: DataType,
                                     args: Seq[Phrase[ExpType]]
                                    ) extends ExpPrimitive {
  (inTs zip args).foreach {
    case (inT, arg) => arg :: expT(inT, read)
  }
  override val t: ExpType = expT(outT, read)

  override def prettyPrint: String = s"${funDecl.name}(${args.map(PrettyPhrasePrinter(_)).mkString(",")})"
}
