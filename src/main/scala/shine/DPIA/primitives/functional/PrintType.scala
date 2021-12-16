package shine.DPIA.primitives.functional

import rise.core.types.{Access, DataType}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

final case class PrintType(msg: String,
                           dt: DataType,
                           access: Access,
                           input: Phrase[ExpType]
                          ) extends ExpPrimitive {
  println(s"$msg : $dt (DPIA level)")

  input :: expT(dt, access)
  override val t: ExpType = expT(dt, access)

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] =
    PrintType(msg, v.data(dt), v.access(access), VisitAndRebuild(input, v))
}
