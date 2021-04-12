package shine.DPIA.primitives.functional

import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Reorder(n: Nat,
                         dt: DataType,
                         access: AccessType,
                         idxF: Phrase[ExpType ->: ExpType],
                         idxFinv: Phrase[ExpType ->: ExpType],
                         input: Phrase[ExpType]
                        ) extends ExpPrimitive {
  idxF :: expT(idx(n), read) ->: expT(idx(n), read)
  idxFinv :: expT(idx(n), read) ->: expT(idx(n), read)
  input :: expT(n`.`dt, access)
  override val t: ExpType = expT(n`.`dt, access)

  override def eval(s: Store): Data = {
    import shine.DPIA.Semantics.OperationalSemantics._
    val idxFE = OperationalSemantics.eval(s, idxF)
    OperationalSemantics.eval(s, input) match {
      case ArrayData(a) =>
        val res = new scala.Array[Data](a.length)
        for (i <- a.indices) {
          res(i) = a(OperationalSemantics.evalIndexExp(s, idxFE(i)).eval)
        }
        ArrayData(res.toVector)
      case _ => throw new Exception("This should not happen")
    }
  }
}
