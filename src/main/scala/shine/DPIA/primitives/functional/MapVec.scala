package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.intermediate.MapVecI
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class MapVec(n: Nat,
                        dt1: ScalarType,
                        dt2: ScalarType,
                        f: Phrase[ExpType ->: ExpType],
                        array: Phrase[ExpType]
                       ) extends ExpPrimitive with ConT with AccT {
  f :: expT(dt1, read) ->: expT(dt2, write)
  array :: expT(vec(n, dt1), read)
  override val t: ExpType = expT(vec(n, dt2), write)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    con(array)(λ(expT(vec(n, dt1), read))(x =>
      MapVecI(n, dt1, dt2, λ(expT(dt1, read))(x => λ(accT(dt2))(o => acc(f(x))(o))), x, A)))

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] = {
    println("WARNING: map loop continuation translation allocates memory")
    // TODO should be removed
    `new`(vec(n, dt2),
      λ(varT(vec(n, dt2)))(tmp =>
        acc(this)(tmp.wr) `;`
          C(tmp.rd))
    )
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
