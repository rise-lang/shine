package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional.NatAsIndex
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

  override def eval(s: Store): Store = {
    val nE = evalIndexExp(s, NatAsIndex(n, Natural(n)))
    val bodyE = OperationalSemantics.eval(s, loopBody)
    (0 until nE.eval).foldLeft(s)((s1, i) =>
      OperationalSemantics.eval(s1, bodyE(Literal(i)))
    )
  }
}
