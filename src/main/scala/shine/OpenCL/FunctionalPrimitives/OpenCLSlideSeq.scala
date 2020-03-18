package shine.OpenCL.FunctionalPrimitives

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

// performs a sequential slide,
// taking advantage of the space/time overlapping reuse opportunity
final case class OpenCLSlideSeq(
  rot: lp.SlideSeq.Rotate,
  a: AddressSpace,
  n: Nat,
  sz: Nat,
  sp: Nat,
  dt1: DataType,
  dt2: DataType,
  load: Phrase[ExpType ->: ExpType],
  input: Phrase[ExpType]
) extends ExpPrimitive
{
  val inputSize: Nat with SimplifiedExpr = sp * n + sz - sp

  load :: expT(dt1, read) ->: expT(dt2, write)
  input :: expT(inputSize`.`dt1, read)
  override val t: ExpType = expT(n`.`(sz`.`dt2), write)

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    OpenCLSlideSeq(rot,
      v.addressSpace(a), v.nat(n), v.nat(sz), v.nat(sp),
      v.data(dt1), v.data(dt2),
      VisitAndRebuild(load, v),
      VisitAndRebuild(input, v))
  }

  override def eval(s: Store): Data = {
    import shine.DPIA.FunctionalPrimitives._
    Slide(n, sz, sp, dt2, Map(inputSize, dt1, dt2, load, input)).eval(s)
  }

  override def acceptorTranslation(A: Phrase[AccType])
    (implicit context: TranslationContext)
  : Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???

  override def streamTranslation(
    C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._
    import shine.OpenCL.IntermediatePrimitives.{
      OpenCLSlideSeqIValues, OpenCLSlideSeqIIndices
    }

    val I = rot match {
      case lp.SlideSeq.Values => OpenCLSlideSeqIValues.apply _
      case lp.SlideSeq.Indices => OpenCLSlideSeqIIndices.apply _
    }

    val i = NatIdentifier(freshName("i"))
    str(input)(fun((i: NatIdentifier) ->:
      (expT(dt1, read) ->: (comm: CommType)) ->: (comm: CommType)
    )(nextIn =>
      I(a, n, sz, sp, dt1, dt2,
        fun(expT(dt1, read))(x =>
          fun(accT(dt2))(o => acc(load(x))(o))),
        nextIn, C)
    ))
  }

  override def prettyPrint: String =
    s"(slideSeq $sz $sp ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <slideSeq n={ToString(n)} sz={ToString(sz)} sp={ToString(sp)}>
      <input>{Phrases.xmlPrinter(input)}</input>
    </slideSeq>
}