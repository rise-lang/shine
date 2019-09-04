package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Types._
import idealised.DPIA.DSL._
import idealised.DPIA._
import idealised.DPIA.Compilation._
import idealised.DPIA.Semantics.OperationalSemantics._

import scala.xml.Elem
import scala.language.reflectiveCalls

final case class IndexAsNat(n: Nat, e: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (n: Nat) ->: (e :: exp"[idx($n)]") ->: exp"[$NatType]"

  def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(e)})"

  override def xmlPrinter: Elem =
    <asNat>
      {Phrases.xmlPrinter(e)}
    </asNat>

  def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] =
    IndexAsNat(fun.nat(n), VisitAndRebuild(e, fun))

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data =
    OperationalSemantics.eval(s, e) match {
      case IndexData(m, _) => NatData(m)
      case d => throw new Exception(s"Expected IndexData but found $d.")
    }

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(e)(λ(exp"[${IndexType(n)}]")(x =>
      A :=|NatType| IndexAsNat(n, x)))
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] = ???

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(e)(λ(exp"[${IndexType(n)}]")(x =>
      C(IndexAsNat(n, x))))
  }
}
