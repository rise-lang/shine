package shine.OpenCL.primitives.functional

import arithexpr.arithmetic.SimplifiedExpr
import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class OpenCLRotateValues(
  a: AddressSpace,
  n: Nat,
  sz: Nat,
  dt: DataType,
  write_dt: Phrase[ExpType ->: ExpType],
  input: Phrase[ExpType]
) extends ExpPrimitive
{
  val sp = 1
  val inputSize: Nat with SimplifiedExpr = sp * n + sz - sp

  write_dt :: expT(dt, read) ->: expT(dt, write)
  input :: expT(inputSize`.`dt, read)
  override val t: ExpType = expT(n`.`(sz`.`dt), read)

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    OpenCLRotateValues(
      v.addressSpace(a), v.nat(n), v.nat(sz), v.data(dt),
      VisitAndRebuild(write_dt, v),
      VisitAndRebuild(input, v))
  }

  override def eval(s: Store): Data = {
    import shine.DPIA.primitives.functional.Slide
    Slide(n, sz, 1, dt, input).eval(s)
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
    import shine.OpenCL.primitives.intermediate.OpenCLRotateValuesI

    val i = NatIdentifier(freshName("i"))
    str(input)(fun((i: NatIdentifier) ->:
      (expT(dt, read) ->: (comm: CommType)) ->: (comm: CommType)
    )(nextIn =>
      OpenCLRotateValuesI(a, n, sz, dt,
        fun(expT(dt, read))(x =>
          fun(accT(dt))(o => acc(write_dt(x))(o))),
        nextIn, C)
    ))
  }

  override def prettyPrint: String =
    s"(rotateValues $sz ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <rotateValues n={ToString(n)} sz={ToString(sz)}>
      <input>{Phrases.xmlPrinter(input)}</input>
    </rotateValues>
}