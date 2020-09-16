package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.TakeAcc
import shine.DPIA.Phrases.{ExpPrimitive, Identifier, Phrase, VisitAndRebuild}
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, _}
import shine.DPIA.Types.DataType._
import shine.DPIA.{->:, Nat, Phrases, _}

import scala.xml.Elem

final case class PadEmpty(
  n: Nat,
  r: Nat,
  dt: DataType,
  array: Phrase[ExpType]
) extends ExpPrimitive {

  array :: expT(n`.`dt, write)
  override val t: ExpType = expT({n + r}`.`dt, write)

  override def eval(s: Store): Data = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    PadEmpty(fun.nat(n), fun.nat(r), fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._
    acc(array)(TakeAcc(n, r, dt, A))
  }

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???

  override def fedeTranslation(
    env: Predef.Map[Identifier[ExpType], Identifier[AccType]])(
    C: Phrase[AccType ->: AccType]
  ): Phrase[AccType] = {
    import TranslationToImperative._
    val otype = C.t.inT.dataType
    fedAcc(env)(array)(fun(accT(otype))(o => TakeAcc(n, r, dt, C(o))))
  }

  override def xmlPrinter: Elem =
    <roundUp n={n.toString} r={r.toString} dt={dt.toString}>
      {Phrases.xmlPrinter(array)}
    </roundUp>

  override def prettyPrint: String = s"(roundUp $array)"
}
