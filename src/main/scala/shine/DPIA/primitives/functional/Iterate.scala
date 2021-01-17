package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.intermediate.IterateIAcc
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Iterate(n: Nat,
                         m: Nat,
                         k: Nat,
                         dt: DataType,
                         f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                         array: Phrase[ExpType]
                        ) extends ExpPrimitive with AcceptorTranslatable {
  {
    val l = f.t.x
    f :: l ->: expT((l * n)`.`dt, read) ->: expT(l`.`dt, write)
    array :: expT((m * n.pow(k))`.`dt, read)
  }
  override val t: ExpType = expT(m`.`dt, write)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] =
    con(array)(λ(expT((m * n.pow(k))`.`dt, read))(x =>
      IterateIAcc(n, m, k, dt, A,
        _Λ_[NatKind]()(l => λ(accT(l `.` dt))(o =>
          λ(expT((l * n)`.`dt, read))(x => acc(f(l)(x))(o)))),
        x)))
}
