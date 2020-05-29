package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.{IndexData, NatData}
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class NatAsIndex(n: Nat, e: Phrase[ExpType])
  extends ExpPrimitive {

  e :: expT(NatType, read)
  override val t: ExpType = expT(idx(n), read)

  def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(e)})"

  override def xmlPrinter: Elem =
    <unsafeAsIndex n={ToString(n)}>
      {Phrases.xmlPrinter(e)}
    </unsafeAsIndex>

  def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] =
    NatAsIndex(fun.nat(n), VisitAndRebuild(e, fun))

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = {
    OperationalSemantics.eval(s, e) match {
      case NatData(m) => IndexData(m, n)
      case d => throw new Exception(s"Expected NatData but found $d.")
    }
  }

  def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] =
    ???

  def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._

    con(e)(λ(e.t)(x =>
      C(NatAsIndex(n, x))))
  }
}
