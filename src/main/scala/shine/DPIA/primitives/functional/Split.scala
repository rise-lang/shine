package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Split(n: Nat,
                       m: Nat,
                       w: AccessType,
                       dt: DataType,
                       array: Phrase[ExpType]
                      ) extends ExpPrimitive {
  array :: expT((m * n)`.`dt, w)
  override val t: ExpType = expT(m`.`(n`.`dt), w)

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, array) match {
      case ArrayData(arrayE) =>
        def split[T](n: Int, vector: Vector[T]): Vector[Vector[T]] = {
          val builder = Vector.newBuilder[Vector[T]]
          var vec = vector
          while (vec.nonEmpty) {
            builder += vec.take(n)
            vec = vec.drop(n)
          }
          builder.result()
        }
        ArrayData(split(n.eval, arrayE).map(ArrayData))
      case _ => throw new Exception("This should not happen")
    }
  }
}
