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
import shine.DPIA.primitives.intermediate.ReduceSeqI
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class ReduceSeq(unroll: Boolean)
                          (val n: Nat,
                           val dt1: DataType,
                           val dt2: DataType,
                           val f: Phrase[ExpType ->: ExpType ->: ExpType],
                           val init: Phrase[ExpType],
                           val array: Phrase[ExpType]
                          ) extends ExpPrimitive with ConT with AccT {
  f :: expT(dt2, read) ->: expT(dt1, read) ->: expT(dt2, write)
  init :: expT(dt2, write)
  array :: expT(n`.`dt1, read)
  override val t: ExpType = expT(dt2, read)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    con(this)(λ(expT(dt2, write))(r =>
      acc(r)(A)))

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(array)(λ(expT(n`.`dt1, read))(X =>
      ReduceSeqI(n, dt1, dt2,
        λ(expT(dt2, read))(x =>
          λ(expT(dt1, read))(y =>
            λ(accT(dt2))(o => acc(f(x)(y))(o)))),
        init, X, C, unroll)(context)))

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
