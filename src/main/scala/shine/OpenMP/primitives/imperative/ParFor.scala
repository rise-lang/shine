package shine.OpenMP.primitives.imperative

import shine.DPIA.Phrases.{CommandPrimitive, Phrase, _}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class ParFor(n: Nat,
                        dt: DataType,
                        out: Phrase[AccType],
                        body: Phrase[ExpType ->: AccType ->: CommType]
                       ) extends CommandPrimitive {
  out :: accT(n`.`dt)
  body :: expT(idx(n), read) ->: accT(dt) ->: comm

  lazy val unwrapBody: (Identifier[ExpType], Identifier[AccType], Phrase[CommType]) = body match {
    case Lambda(i, Lambda(o, body)) => (i, o, body)
    case _ => throw new Exception("This should not happen")
  }
}
