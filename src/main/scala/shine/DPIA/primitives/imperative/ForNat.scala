package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class ForNat(unroll: Boolean)
                       (val n: Nat,
                        val loopBody: Phrase[`(nat)->:`[CommType]]
                       ) extends CommandPrimitive {
  loopBody :: loopBody.t.x ->: comm

  lazy val unwrapBody: (NatIdentifier, Phrase[CommType]) = loopBody match {
    case DepLambda(i, body) => (i, body)
    case _ => throw new Exception("This should not happen")
  }
}
