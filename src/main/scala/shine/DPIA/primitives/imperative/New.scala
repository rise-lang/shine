package shine.DPIA.primitives.imperative

import shine.DPIA.DSL.identifier
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class New(dt: DataType,
                     f: Phrase[VarType ->: CommType]
                    ) extends CommandPrimitive {
  f :: varT(dt) ->: comm

  override def eval(s: Store): Store = {
    val f_ = OperationalSemantics.eval(s, f)
    val arg = identifier(freshName("x"), f.t.inT)
    val newStore = OperationalSemantics.eval(s + (arg.name -> 0), f_(arg))
    newStore - arg.name
  }
}
