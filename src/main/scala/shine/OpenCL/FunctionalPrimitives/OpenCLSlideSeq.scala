package shine.OpenCL.FunctionalPrimitives

import arithexpr.arithmetic.SimplifiedExpr
import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

// TODO: use separate primitives just like in rise
object OpenCLSlideSeq {
  trait Rotate {}
  case object Values extends Rotate {}
  case object Indices extends Rotate {}
}

// performs a sequential slide,
// taking advantage of the space/time overlapping reuse opportunity
final case class OpenCLSlideSeq(rot: OpenCLSlideSeq.Rotate,
                                a: AddressSpace,
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
    OpenCLSlideSeq(rot,
      v.addressSpace(a), v.nat(n), v.nat(sz), v.nat(sp),
      v.data(dt),
      VisitAndRebuild(write_dt, v),
      VisitAndRebuild(input, v))
  }

  override def eval(s: Store): Data = {
    import shine.DPIA.FunctionalPrimitives._
    Slide(n, sz, sp, dt, input).eval(s)
  }

  override def acceptorTranslation(A: Phrase[AccType])
    (implicit context: TranslationContext)
  : Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def streamTranslation(C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])
                                (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    import shine.OpenCL.IntermediatePrimitives.{
      OpenCLSlideSeqIValues, OpenCLSlideSeqIIndices
    }

    val I = rot match {
      case OpenCLSlideSeq.Values => OpenCLSlideSeqIValues.apply _
      case OpenCLSlideSeq.Indices => OpenCLSlideSeqIIndices.apply _
    }

    con(input)(fun(expT(inputSize`.`dt, read))(x =>
      I(a, n, sz, sp, dt,
        fun(expT(dt, read))(x =>
          fun(accT(dt))(o => acc(write_dt(x))(o))),
        x, C)
    ))
  }

  override def prettyPrint: String =
    s"(slideSeq $sz $sp ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <slideSeq n={ToString(n)} sz={ToString(sz)} sp={ToString(sp)} dt={ToString(dt)}>
      <input>{Phrases.xmlPrinter(input)}</input>
    </slideSeq>
}