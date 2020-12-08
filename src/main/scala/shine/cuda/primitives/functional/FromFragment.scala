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

case class FromFragment(ldm: Nat,
                        m: Nat,
                        n: Nat,
                        k: Nat,
                        dataType: DataType,
                        fragment: Phrase[ExpType],
                        layout: MatrixLayout
                       ) extends ExpPrimitive {

  fragment :: ExpType(WmmaAccumulator(m, n, k, dataType), read)
  val fragArrayType = fragment.t.dataType.asInstanceOf[WmmaFragment].arrayType
  override val t: ExpType = ExpType(fragArrayType, write)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    FromFragment(f.nat(ldm), f.nat(m), f.nat(n), f.nat(k), f.data(dataType),
      VisitAndRebuild(fragment, f), layout)
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    con(fragment)(λ(ExpType(fragment.t.dataType, read))(fragment =>
      WmmaStore(ldm, m, n, k,
        dataType, fragment, A, layout)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = ???

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"FromFragment($ldm, ${PrettyPhrasePrinter(fragment)}, $layout)"

  override def xmlPrinter: Elem =
    <FromFragment ldm={ToString(ldm)} layout={ToString(layout)}>
      <fragment>
        {Phrases.xmlPrinter(fragment)}
      </fragment>
    </FromFragment>
}
