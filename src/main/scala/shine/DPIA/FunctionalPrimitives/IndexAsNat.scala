package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, _}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.{IndexData, NatData}
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.language.reflectiveCalls
import scala.xml.Elem

final case class IndexAsNat(n: Nat, e: Phrase[ExpType])
  extends ExpPrimitive {

  e :: expT(idx(n), read)
  override val t: ExpType = expT(NatType, read)

  def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(e)})"

  override def xmlPrinter: Elem =
    <asNat>
      {Phrases.xmlPrinter(e)}
    </asNat>

  def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] =
    IndexAsNat(fun.nat(n), VisitAndRebuild(e, fun))

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = {
    OperationalSemantics.eval(s, e) match {
      case IndexData(i, _) => NatData(i)
      case d => throw new Exception(s"Expected IndexData but found $d.")
    }
  }

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(e)(λ(expT(idx(n), read))(x =>
      A :=|NatType| IndexAsNat(n, x)))
  }

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(e)(λ(expT(idx(n), read))(x =>
      C(IndexAsNat(n, x))))
  }
}
