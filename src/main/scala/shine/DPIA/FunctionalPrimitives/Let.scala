package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

final case class Let(dt1: DataType,
                     dt2: DataType,
                     access: AccessType,
                     value: Phrase[ExpType],
                     f: Phrase[ExpType ->: ExpType])
  extends ExpPrimitive
{
  override val t: ExpType =
    (dt1: DataType) ->: (dt2: DataType) ->: (access: AccessType) ->:
      (value :: exp"[$dt1, $read]") ->:
      (f :: t"exp[$dt1, $read] -> exp[$dt2, $access]") ->:
      exp"[$dt2, $access]"

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] =
    Let(v.data(dt1), v.data(dt2), v.access(access),
      VisitAndRebuild(value, v),
      VisitAndRebuild(f, v))

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import shine.DPIA.Compilation.TranslationToImperative._
    con(value)(fun(value.t)(x => acc(f(x))(A)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    import shine.DPIA.Compilation.TranslationToImperative._
    con(value)(fun(value.t)(x => con(f(x))(C)))
  }

  override def prettyPrint: String = s"(let ${PrettyPhrasePrinter(value)} ${PrettyPhrasePrinter(f)})"

  override def xmlPrinter: Elem =
    <let dt1={ToString(dt1)} dt2={ToString(dt2)} access={ToString(access)}>
      <value>{Phrases.xmlPrinter(value)}</value>
      <f>{Phrases.xmlPrinter(f)}</f>
    </let>
}
