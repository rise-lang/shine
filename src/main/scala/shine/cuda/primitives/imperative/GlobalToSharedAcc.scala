package shine.cuda.primitives.imperative

import shine.DPIA.DSL.{`new` => _}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class GlobalToSharedAcc(dt: DataType,
                                   pipe: Phrase[ExpType],
                                   outputShared: Phrase[AccType]
                                  ) extends AccPrimitive {
  pipe :: expT(pipeline, read)
  outputShared :: accT(dt)
  override val t: AccType = accT(dt)

  override def prettyPrint: String =
    s"(GlobalToSharedAcc $pipe, ${PrettyPhrasePrinter(outputShared)})"
}


