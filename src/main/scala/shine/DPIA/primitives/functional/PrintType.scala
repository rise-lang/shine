package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class PrintType(msg: String,
                           dt: DataType,
                           access: AccessType,
                           input: Phrase[ExpType]
                          ) extends ExpPrimitive {
  println(s"$msg : $dt (DPIA level)")

  input :: expT(dt, access)
  override val t: ExpType = expT(dt, access)

  override def eval(s: Store): Data = OperationalSemantics.eval(s, input)
}
