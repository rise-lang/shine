package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Types._
import idealised.DPIA.DSL._
import idealised.DPIA.Compilation._
import idealised.DPIA._

import scala.xml.Elem

final case class Cast(dt1: BasicType, dt2: BasicType, e: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (dt1: BasicType) -> (dt2: BasicType) ->
      (e :: exp"[$dt1, $read]") -> exp"[$dt2, $read]"

  def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(e)})"

  override def xmlPrinter: Elem =
    <cast dt1={ToString(dt1)} dt2={ToString(dt2)}>
      {Phrases.xmlPrinter(e)}
    </cast>

  def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] =
    Cast(fun(dt1), fun(dt2), VisitAndRebuild(e, fun))

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = ???

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommandType] = ???

  override def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommandType] = ???

  def continuationTranslation(C: Phrase[ExpType -> CommandType])
                             (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(e)(fun(e.t)(x =>
      C(Cast(dt1, dt2, x))))
  }
}
