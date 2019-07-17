package idealised.DPIA.FunctionalPrimitives

import lift.core.{primitives => lp}
import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA._
import idealised.DPIA.DSL._
import idealised.DPIA.Types._
import idealised.DPIA.Phrases._

// performs a sequential slide, taking advantage of the space/time overlapping reuse opportunity
final case class SlideSeq(rot: lp.slideSeq.Rotate,
                          n: Nat,
                          sz: Nat,
                          sp: Nat,
                          dt: DataType,
                          input: Phrase[ExpType])
  extends AbstractSlide(n, sz, sp, dt, input)
{
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    SlideSeq(rot, v.nat(n), v.nat(sz), v.nat(sp), v.data(dt), VisitAndRebuild(input, v))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    mapAcceptorTranslation(fun(exp"[$sz.$dt]")(x => x), A)
  }

  override def mapAcceptorTranslation(g: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    import idealised.DPIA.IntermediatePrimitives.{SlideSeqIValues, SlideSeqIIndices}

    val I = rot match {
      case lp.slideSeq.Values => SlideSeqIValues.apply _
      case lp.slideSeq.Indices => SlideSeqIIndices.apply _
    }

    con(input)(fun(exp"[$inputSize.$dt]")(x =>
      I(n, sz, sp, dt, g.t.outT.dataType,
        fun(exp"[$sz.$dt]")(x =>
          fun(acc"[${g.t.outT.dataType}]")(o => acc(g(x))(o))),
        x, A
      )))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    `new`(dt"[$n.$dt]", fun(exp"[$n.$dt]" x acc"[$n.$dt]")(tmp =>
      acc(this)(tmp.wr) `;` C(tmp.rd)
    ))
  }
}