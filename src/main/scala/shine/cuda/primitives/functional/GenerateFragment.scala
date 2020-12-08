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
                            fill: Phrase[ExpType]) extends ExpPrimitive {

  fill :: ExpType(dataType, read)

  override val t: ExpType = ExpType(WmmaAccumulator(m, n, k, dataType), read)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    GenerateFragment(f.nat(m), f.nat(n), f.nat(k), f.data(dataType),
      VisitAndRebuild(fill, f))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    con(fill)(λ(ExpType(dataType, read))(fill =>
      WmmaFill(m, n, k, dataType, fill, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    OpenCLNew(AddressSpace.Private, WmmaAccumulator(m, n, k, dataType),
      λ(VarType(WmmaAccumulator(m, n, k, dataType)))(fragment =>
        acceptorTranslation(fragment.wr) `;`
          C(fragment.rd)))
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String =
    s"GenerateFragment($m, $n, $k, ${PrettyPhrasePrinter(fill)})"

  override def xmlPrinter: Elem =
    <GenerateFragment n={ToString(n)} m={ToString(m)} k={ToString(k)} dt1={ToString(dataType)}>
      <fill>
        {Phrases.xmlPrinter(fill)}
      </fill>
    </GenerateFragment>
}
