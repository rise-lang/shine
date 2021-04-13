package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class ReduceSeq(unroll: Boolean)
                          (val n: Nat,
                           val dt1: DataType,
                           val dt2: DataType,
                           val f: Phrase[ExpType ->: ExpType ->: ExpType],
                           val init: Phrase[ExpType],
                           val array: Phrase[ExpType]
                          ) extends ExpPrimitive {
  f :: expT(dt2, read) ->: expT(dt1, read) ->: expT(dt2, write)
  init :: expT(dt2, write)
  array :: expT(n`.`dt1, read)
  override val t: ExpType = expT(dt2, read)

  def unwrap: (Nat, DataType, DataType, Phrase[ExpType ->: ExpType ->: ExpType], Phrase[ExpType], Phrase[ExpType]) =
    (n, dt1, dt2, f, init, array)

  override def eval(s: Store): Data = {
    val fE = OperationalSemantics.eval(s, f)(BinaryFunctionEvaluator)
    val initE = OperationalSemantics.eval(s, init)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(Vector(xs.fold(initE) {
          (x, y) => OperationalSemantics.eval(s,
            fE(Literal(x))(Literal(y)))
        }))
      case _ => throw new Exception("This should not happen")
    }
  }
}
