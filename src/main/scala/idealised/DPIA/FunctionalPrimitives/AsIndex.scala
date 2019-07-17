package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Types._
import idealised.DPIA.DSL._
import idealised.DPIA.Semantics.OperationalSemantics.{IndexData, NatData}
import idealised.DPIA._

import scala.xml.Elem
import scala.language.reflectiveCalls

final case class AsIndex(n: Nat, e: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (n: Nat) -> (e :: exp"[$NatType]") -> exp"[${IndexType(n)}]"

  def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(e)})"

  override def xmlPrinter: Elem =
    <unsafeAsIndex n={ToString(n)}>
      {Phrases.xmlPrinter(e)}
    </unsafeAsIndex>

  def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] =
    AsIndex(fun.nat(n), VisitAndRebuild(e, fun))

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = {
    OperationalSemantics.eval(s, e) match {
      case NatData(m) => IndexData(m, IndexType(n))
      case d => throw new Exception(s"Expected NatData but found $d.")
    }
  }

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(e)(fun(exp"[$NatType]")(x =>
      A :=|IndexType(n)| AsIndex(n, x)))
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] = ???

  def continuationTranslation(C: Phrase[ExpType -> CommType])
                             (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(e)(λ(e.t)(x =>
      C(AsIndex(n, x))))
  }
}
