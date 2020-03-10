package shine.DPIA.FunctionalPrimitives

import arithexpr.arithmetic.SimplifiedExpr
import rise.core.{primitives => lp}
import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

// performs a sequential slide, taking advantage of the space/time overlapping reuse opportunity
final case class SlideSeq(rot: lp.SlideSeq.Rotate,
                          n: Nat,
                          sz: Nat,
                          sp: Nat,
                          dt: DataType,
                          write_dt: Phrase[ExpType ->: ExpType],
                          input: Phrase[ExpType])
  extends ExpPrimitive
{
  val inputSize: Nat with SimplifiedExpr = sp * n + sz - sp

  write_dt :: expT(dt, read) ->: expT(dt, write)
  input :: expT(inputSize`.`dt, read)
  override val t: ExpType = expT(n`.`(sz`.`dt), read)

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    SlideSeq(rot, v.nat(n), v.nat(sz), v.nat(sp), v.data(dt),
      VisitAndRebuild(write_dt, v),
      VisitAndRebuild(input, v))
  }

  override def eval(s: Store): Data = {
    Slide(n, sz, sp, dt, input).eval(s)
  }

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???

  override def streamTranslation(
    C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._
    import shine.DPIA.IntermediatePrimitives.{
      SlideSeqIValues, SlideSeqIIndices
    }

    val I = rot match {
      case lp.SlideSeq.Values => SlideSeqIValues.apply _
      case lp.SlideSeq.Indices => SlideSeqIIndices.apply _
    }

    val i = NatIdentifier(freshName("i"))
    str(input)(fun((i: NatIdentifier) ->:
      (expT(dt, read) ->: (comm: CommType)) ->: (comm: CommType)
    )(nextIn =>
      I(n, sz, sp, dt,
        fun(expT(dt, read))(x =>
          fun(accT(dt))(o => acc(write_dt(x))(o))),
        nextIn, C)
    ))
  }

  override def prettyPrint: String = s"slideSeq"

  override def xmlPrinter: Elem = <slideSeq></slideSeq>
}