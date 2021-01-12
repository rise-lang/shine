package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.con
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{->:, Nat, Phrases}
import shine.cuda.primitives.imperative.WmmaStore

import scala.xml.Elem

case class FromFragment(m: Nat,
                        n: Nat,
                        k: Nat,
                        dataType: DataType,
                        fragment: Phrase[ExpType]
                       ) extends ExpPrimitive {

  fragment :: ExpType(Fragment(m, n, k, dataType), read)
  val fragArrayType = fragment.t.dataType.asInstanceOf[Fragment].matrixType
  override val t: ExpType = ExpType(fragArrayType, write)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    FromFragment(f.nat(m), f.nat(n), f.nat(k), f.data(dataType),
      VisitAndRebuild(fragment, f))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    con(fragment)(λ(ExpType(fragment.t.dataType, read))(fragment =>
      WmmaStore(m, n, k,
        dataType, fragment, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = ???

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"FromFragment(${PrettyPhrasePrinter(fragment)})"

  override def xmlPrinter: Elem =
    <FromFragment>
      <fragment>
        {Phrases.xmlPrinter(fragment)}
      </fragment>
    </FromFragment>
}
