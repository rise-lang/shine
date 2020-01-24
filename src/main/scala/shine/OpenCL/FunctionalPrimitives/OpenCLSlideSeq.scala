package shine.OpenCL.FunctionalPrimitives

import arithexpr.arithmetic.SimplifiedExpr
import rise.core.{primitives => lp}
import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

// performs a sequential slide, taking advantage of the space/time overlapping reuse opportunity
final case class OpenCLSlideSeq(rot: lp.SlideSeq.Rotate,
                                a: AddressSpace,
                                n: Nat,
                                sz: Nat,
                                sp: Nat,
                                dt1: DataType,
                                dt2: DataType,
                                write_dt1: Phrase[ExpType ->: ExpType],
                                f: Phrase[ExpType ->: ExpType],
                                input: Phrase[ExpType])
  extends ExpPrimitive
{
  val inputSize: Nat with SimplifiedExpr = sp * n + sz - sp

  override val t: ExpType =
    (a: AddressSpace) ->: (n: Nat) ->: (sz: Nat) ->: (sp: Nat) ->: (dt1: DataType) ->: (dt2: DataType) ->:
      (write_dt1 :: t"exp[$dt1, $read] -> exp[$dt1, $write]") ->:
      (f :: t"exp[$sz.$dt1, $read] -> exp[$dt2, $write]") ->:
      (input :: exp"[$inputSize.$dt1, $read]") ->:
      exp"[$n.$dt2, $write]"

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    OpenCLSlideSeq(rot,
      v.addressSpace(a), v.nat(n), v.nat(sz), v.nat(sp), v.data(dt1), v.data(dt2),
      VisitAndRebuild(write_dt1, v),
      VisitAndRebuild(f, v),
      VisitAndRebuild(input, v))
  }

  override def eval(s: Store): Data = {
    //import shine.DPIA.FunctionalPrimitives._
    //FIXME (access type)
    //Map(n, ArrayType(sz, dt1), dt2, f, Slide(n, sz, sp, dt1, input)).eval(s)
    ???
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    import shine.OpenCL.IntermediatePrimitives.OpenCLSlideSeqIValues

    val I = rot match {
      case lp.SlideSeq.Values => OpenCLSlideSeqIValues.apply _
      case lp.SlideSeq.Indices => ??? // SlideSeqIIndices.apply _
    }

    con(input)(fun(exp"[$inputSize.$dt1, $read]")(x =>
      I(a, n, sz, sp, dt1, dt2,
        fun(exp"[$dt1, $read]")(x =>
          fun(acc"[$dt1]")(o => acc(write_dt1(x))(o))),
        fun(exp"[$sz.$dt1, $read]")(x =>
          fun(acc"[$dt2]")(o => acc(f(x))(o))),
        x, A
      )))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def prettyPrint: String =
    s"(slideSeq $sz $sp ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <slideSeq n={ToString(n)} sz={ToString(sz)} sp={ToString(sp)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f>{Phrases.xmlPrinter(f)}</f>
      <input>{Phrases.xmlPrinter(input)}</input>
    </slideSeq>
}