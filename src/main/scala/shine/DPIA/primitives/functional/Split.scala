package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.imperative.SplitAcc
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Split(n: Nat,
                       m: Nat,
                       w: AccessType,
                       dt: DataType,
                       array: Phrase[ExpType]
                      ) extends ExpPrimitive with ContinuationTranslatable with AcceptorTranslatable {
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

  override def fedeTranslation(env: Predef.Map[Identifier[ExpType], Identifier[AccType]])
                              (C: Phrase[AccType ->: AccType]): Phrase[AccType] =
    fedAcc(env)(array)(λ(accT(C.t.inT.dataType))(o =>
      SplitAcc(n, m, dt, C(o))))

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    acc(array)(SplitAcc(n, m, dt, A))

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(array)(λ(expT((m * n)`.`dt, read))(x =>
      C(Split(n, m, w, dt, x))))
}
