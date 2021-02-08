package shine.DPIA.primitives.imperative

import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional.NatAsIndex
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class ForVec(n: Nat,
                        dt: ScalarType,
                        out: Phrase[AccType],
                        loopBody: Phrase[ExpType ->: AccType ->: CommType]
                       ) extends CommandPrimitive {
  out :: accT(vec(n, dt))
  loopBody :: expT(idx(n), read) ->: accT(dt) ->: comm

  override def eval(s: Store): Store = {
    val nE = evalIndexExp(s, NatAsIndex(n, Natural(n)))
    val bodyE = OperationalSemantics.eval(s, loopBody)(OperationalSemantics.BinaryFunctionEvaluator)

    (0 until nE.eval).foldLeft(s)((s1, i) => {
      OperationalSemantics.eval(s1,
        bodyE(Literal(i))(out `@` Literal(i)))
    })
  }
}
