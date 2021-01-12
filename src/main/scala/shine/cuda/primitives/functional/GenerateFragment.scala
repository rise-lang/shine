package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.con
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{->:, Nat, Phrases, VarType}
import shine.OpenCL.ImperativePrimitives.OpenCLNew
import shine.cuda.primitives.imperative.WmmaFill

import scala.xml.Elem

case class GenerateFragment(m: Nat,
                            n: Nat,
                            k: Nat,
                            dataType: DataType,
                            fill: Phrase[ExpType],
                            fragmentType: FragmentType,
                            layout: MatrixLayout) extends ExpPrimitive {

  fill :: ExpType(dataType, read)

  override val t: ExpType = ExpType(Fragment(m, n, k, dataType), write)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    GenerateFragment(f.nat(m), f.nat(n), f.nat(k), f.data(dataType),
      VisitAndRebuild(fill, f), fragmentType, layout)
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    con(fill)(λ(ExpType(dataType, read))(fill =>
      WmmaFill(m, n, k, dataType, fill, fragmentType, layout, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = ???

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String =
    s"GenerateFragment($m, $n, $k, ${PrettyPhrasePrinter(fill)}, $fragmentType, $layout)"

  override def xmlPrinter: Elem =
    <GenerateFragment m={ToString(m)} n={ToString(n)} k={ToString(k)} dt={ToString(dataType)}
                      fragType={ToString(fragmentType)} layout={ToString(layout)}>
      <fill>
        {Phrases.xmlPrinter(fill)}
      </fill>
    </GenerateFragment>
}
