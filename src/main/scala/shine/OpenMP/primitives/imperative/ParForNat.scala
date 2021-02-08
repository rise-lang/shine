package shine.OpenMP.primitives.imperative

import shine.DPIA.Phrases.{CommandPrimitive, Phrase, _}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class ParForNat(n: Nat,
                           ft: NatToData,
                           out: Phrase[AccType],
                           body: Phrase[`(nat)->:`[AccType ->: CommType]]
                          ) extends CommandPrimitive {
  out :: accT(n`.d`ft)
  body :: body.t.x ->: accT(ft(body.t.x)) ->: comm

  lazy val unwrapBody: (NatIdentifier, Identifier[AccType], Phrase[CommType]) = body match {
    case DepLambda(n, Lambda(o, body)) => (n, o, body)
    case _ => throw new Exception("This should not happen")
  }
}
