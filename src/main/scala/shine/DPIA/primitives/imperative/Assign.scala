package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.comPrimitive

@comPrimitive
case class Assign(dt: DataType,
                  lhs: Phrase[AccType],
                  rhs: Phrase[ExpType]
                 ) extends CommandPrimitive {
  lhs :: accT(dt)
  rhs :: expT(dt, read)

  override def prettyPrint: String = s"(${PrettyPhrasePrinter(lhs)} := ${PrettyPhrasePrinter(rhs)})"
}

