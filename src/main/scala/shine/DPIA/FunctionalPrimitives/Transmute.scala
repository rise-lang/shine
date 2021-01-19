package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.TransmuteAcc
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

final case class Transmute(a: AccessType, inT: DataType, outT:DataType, input: Phrase[ExpType]) extends ExpPrimitive {
  override val t = expT(outT, a)

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(input)(λ(expT(inT, a))(x => C(Transmute(a, inT, outT, x))))
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    acc(input)(TransmuteAcc(inT, outT, A))
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"transmute"

  override def xmlPrinter: Elem = <transmute></transmute>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = Transmute(
    v.access(a),
    v.data(inT),
    v.data(outT),
    VisitAndRebuild(input, v)
  )
}

