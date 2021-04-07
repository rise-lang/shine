package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

// cycles on the m elements of an array (modulo indexing) to produce an array of n elements
@expPrimitive
final case class Cycle(n: Nat,
                       m: Nat,
                       dt: DataType,
                       input: Phrase[ExpType]
                      ) extends ExpPrimitive {
  input :: expT(m`.`dt, read)
  override val t: ExpType = expT(n`.`dt, read)

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, input) match {
      case ArrayData(a) =>
        val N = n.eval
        ArrayData(LazyList.continually(a).flatten.take(N).toVector)
      case _ => throw new Exception("this should not happen")
    }
  }
}
