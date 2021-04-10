package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class For(unroll: Boolean)
                    (val n: Nat,
                     val loopBody: Phrase[ExpType ->: CommType]
                    ) extends CommandPrimitive {
  loopBody :: expT(idx(n), read) ->: comm

  lazy val unwrapBody: (Identifier[ExpType], Phrase[CommType]) = loopBody match {
    case Lambda(i, body) => (i, body)
    case _ => throw new Exception("This should not happen")
  }
}
