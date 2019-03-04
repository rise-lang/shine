package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA._
import idealised.DPIA.DSL._
import idealised.DPIA.Types._
import idealised.DPIA.Phrases._
import idealised.DPIA.Phrases.ExpPrimitive
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.{Data, Store}
import idealised.OpenCL.GlobalMemory

// performs a sequential slide, taking advantage of the space/time overlapping reuse opportunity
final case class SlideSeq(n: Nat,
                          sz: Nat,
                          sp: Nat,
                          dt: DataType,
                          input: Phrase[ExpType])
  extends AbstractSlide(n, sz, sp, dt, input)
{
  assert(sp.eval == 1) // FIXME?

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    SlideSeq(v(n), v(sz), v(sp), v(dt), VisitAndRebuild(input, v))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    mapAcceptorTranslation(A, fun(exp"[$sz.$dt]")(x => x))
  }

  override def mapAcceptorTranslation(A: Phrase[AccType], g: Phrase[ExpType -> ExpType])
                                     (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._
    import idealised.DPIA.IntermediatePrimitives.{MapSeqSlideIRegRot => I} // TODO: making a choice here

    con(input)(fun(exp"[$inputSize.$dt]")(x =>
      I(n, sz, dt, g.t.outT.dataType,
        fun(exp"[$sz.$dt]")(x =>
          fun(acc"[${g.t.outT.dataType}]")(o => acc(g(x))(AccExt(o)))),
        x, A
      )))
  }

  override def continuationTranslation(C: Phrase[->[ExpType, CommandType]])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    `new`(dt"[$n.$dt]", fun(exp"[$n.$dt]" x acc"[$n.$dt]")(tmp =>
      acc(this)(AccExt(tmp.wr)) `;` C(tmp.rd)
    ))
  }
}