package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, _}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

final case class Cast(
  dt1: BasicType,
  dt2: BasicType,
  access: AccessType,
  e: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (dt1: BasicType) ~>: (dt2: BasicType) ~>: (access: AccessType) ~>:
      (e :: expT(dt1, access)) ~>: expT(dt2, access)

  def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(e)})"

  override def xmlPrinter: Elem =
    <cast dt1={ToString(dt1)} dt2={ToString(dt2)} access={ToString(access)}>
      {Phrases.xmlPrinter(e)}
    </cast>

  def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] =
    Cast(fun.data(dt1), fun.data(dt2), fun.access(access),
      VisitAndRebuild(e, fun))

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = ???

  def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???

  def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._

    con(e)(fun(e.t)(x =>
      C(Cast(dt1, dt2, access, x))))
  }
}
