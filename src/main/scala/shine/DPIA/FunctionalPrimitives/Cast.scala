package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, _}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

final case class Cast(dt1: BasicType, dt2: BasicType, e: Phrase[ExpType])
  extends ExpPrimitive {

  //TODO Answer Question: Should this be polymorphic over the access type?
  override val t: ExpType =
    (dt1: BasicType) ->: (dt2: BasicType) ->:
      (e :: exp"[$dt1, $read]") ->: exp"[$dt2, $read]"

  def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(e)})"

  override def xmlPrinter: Elem =
    <cast dt1={ToString(dt1)} dt2={ToString(dt2)}>
      {Phrases.xmlPrinter(e)}
    </cast>

  def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] =
    Cast(fun.data(dt1), fun.data(dt2), VisitAndRebuild(e, fun))

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = ???

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    //FIXME general assignment should be disallowed!
    con(e)(λ(e.t)(x => A :=|dt2| Cast(dt1, dt2, x)) )
  }

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(e)(fun(e.t)(x =>
      C(Cast(dt1, dt2, x))))
  }
}
