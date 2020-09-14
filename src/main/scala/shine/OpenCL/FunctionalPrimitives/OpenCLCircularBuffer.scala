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

final case class OpenCLCircularBuffer(
  a: AddressSpace,
  n: Nat,
  alloc: Nat,
  sz: Nat,
  dt1: DataType,
  dt2: DataType,
  load: Phrase[ExpType ->: ExpType],
  input: Phrase[ExpType]
) extends ExpPrimitive
{
  val inputSize: Nat with SimplifiedExpr = n + sz - 1

  load :: expT(dt1, read) ->: expT(dt2, write)
  input :: expT(inputSize`.`dt1, read)
  override val t: ExpType = expT(n`.`(sz`.`dt2), write)

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    OpenCLCircularBuffer(v.addressSpace(a), v.nat(n), v.nat(alloc), v.nat(sz),
      v.data(dt1), v.data(dt2),
      VisitAndRebuild(load, v),
      VisitAndRebuild(input, v))
  }

  override def eval(s: Store): Data = {
    import shine.DPIA.FunctionalPrimitives._
    Slide(n, sz, 1, dt2, Map(inputSize, dt1, dt2, read, load, input)).eval(s)
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
    import shine.OpenCL.IntermediatePrimitives.OpenCLCircularBufferI

    val i = NatIdentifier(freshName("i"))
    str(input)(fun((i: NatIdentifier) ->:
      (expT(dt1, read) ->: (comm: CommType)) ->: (comm: CommType)
    )(nextIn =>
      OpenCLCircularBufferI(a, n, alloc, sz, dt1, dt2,
        fun(expT(dt1, read))(x =>
          fun(accT(dt2))(o => acc(load(x))(o))),
        nextIn, C)
    ))
  }

  override def prettyPrint: String =
    s"(circularBuffer $alloc $sz ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <circularBuffer n={ToString(n)} sz={ToString(sz)}>
      <input>{Phrases.xmlPrinter(input)}</input>
    </circularBuffer>
}