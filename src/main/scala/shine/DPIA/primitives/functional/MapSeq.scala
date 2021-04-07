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
import shine.DPIA.primitives.intermediate.MapSeqI
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class MapSeq(unroll: Boolean)
                       (val n: Nat,
                        val dt1: DataType,
                        val dt2: DataType,
                        val f: Phrase[ExpType ->: ExpType],
                        val array: Phrase[ExpType]
                       ) extends ExpPrimitive with AccT {
  f :: expT(dt1, read) ->: expT(dt2, write)
  array :: expT(n`.`dt1, read)
  override val t: ExpType = expT(n`.`dt2, write)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] = {
    con(array)(λ(expT(n`.`dt1, read))(x =>
      MapSeqI(unroll)(n, dt1, dt2,
        fun(expT(dt1, read))(x =>
          fun(accT(dt2))(o =>
            acc(f(x))(o))),
        x, A)))
  }

  override def eval(s: Store): Data = {
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(xs.map { x =>
          OperationalSemantics.eval(s, fE(Literal(x)))
        })

      case _ => throw new Exception("This should not happen")
    }
  }
}
