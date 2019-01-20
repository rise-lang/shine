package idealised.OpenMP.ImperativePrimitives

import idealised.DPIA._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA.{->, Nat}

final case class ParForVec(override val n: Nat,
                           override val dt: ScalarType,
                           override val out: Phrase[AccType],
                           override val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends AbstractParFor[ScalarType](n, dt, out, body)
{

  override lazy val `type`: CommandType =
    (n: Nat) -> (dt: ScalarType) ->
      (out :: acc"[${VectorType(n, dt)}]") ->
        (body :: t"exp[idx($n)] -> acc[$dt] -> comm") ->
          comm

  override def makeParFor:
  (Nat, ScalarType, Phrase[AccType], Phrase[->[ExpType, ->[AccType, CommandType]]]) => ParForVec = ParForVec
}
