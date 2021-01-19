package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem


final case class UpdatedAt(n: Nat, dt: DataType, input:Phrase[ExpType], idx: Phrase[ExpType], data:Phrase[ExpType]) extends ExpPrimitive {
  override val t = expT(n `.` dt, write)

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = ???

  // TODO: WHY? How does this work?
  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(input)(λ(expT(n `.` dt, read))(_ =>
      con(idx)(λ(expT(IndexType(n), read))(idx => {
        con(data)(λ(expT(IndexType(n), write))(data => {
          (A `@` idx) :=|dt| data
        }))
      }))))
  }

  override def prettyPrint: String = s"updateAtIdx"

  override def xmlPrinter: Elem = <updateAtIdx></updateAtIdx>

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): UpdatedAt =
    UpdatedAt(v.nat(n), v.data(dt), VisitAndRebuild(input, v), VisitAndRebuild(idx, v), VisitAndRebuild(data, v))
}