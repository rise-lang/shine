package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.intermediate.ScanSeqI
import shine.macros.Primitive.expPrimitive

//noinspection TypeAnnotation
@expPrimitive
final case class ScanSeq(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         f: Phrase[ExpType ->: ExpType ->: ExpType],
                         init: Phrase[ExpType],
                         array: Phrase[ExpType]
                        ) extends ExpPrimitive with ContinuationTranslatable with AcceptorTranslatable {
  f :: expT(dt1, read) ->: expT(dt2, read) ->: expT(dt2, write)
  init :: expT(dt2, write)
  array :: expT(n`.`dt1, read)
  override val t: ExpType = expT(n`.`dt2, read)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    con(array)(λ(expT(n`.`dt1, read))(x =>
      con(init)(λ(expT(dt2, read))(y =>
        ScanSeqI(n, dt1, dt2,
          λ(expT(dt1, read))(x => λ(expT(dt2, read))(y => λ(accT(dt2))(o =>
            acc(f(x)(y))(o)))),
          y, x, A
        )
      ))
    ))

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    `new`(n`.`dt2, λ(varT(n`.`dt2))(tmp =>
      acc(this)(tmp.wr) `;` C(tmp.rd) ))
}
