package shine.OpenMP.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenMP.primitives.intermediate.ReduceParI
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class ReducePar(n: Nat,
                           dt1: DataType, dt2: DataType,
                           f: Phrase[ExpType ->: ExpType ->: ExpType],
                           init: Phrase[ExpType],
                           array: Phrase[ExpType]
                          ) extends ExpPrimitive with ContinuationTranslatable with AcceptorTranslatable {
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
      con(init)(λ(expT(dt2, read))(Y =>
        ReduceParI(n, dt1, dt2,
          λ(expT(dt2, read))(x => λ(expT(dt1, read))(y => λ(accT(dt2))(o =>
            acc( f(x)(y) )( o ) ))),
          Y, X, C)))))

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
