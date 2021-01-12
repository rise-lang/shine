package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.con
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{->:, Nat, Phrases, freshName}
import shine.cuda.primitives.imperative.WmmaLoad

import scala.xml.Elem

object ToFragment{
  def apply(m: Nat, n: Nat, k: Nat, dataType: DataType, fragmentType: FragmentType, matrix: Phrase[ExpType]):
  ToFragment = ToFragment(m, n, k, dataType, fragmentType, matrix, MatrixLayoutIdentifier("ml"))
}

case class ToFragment(m: Nat,
                      n: Nat,
                      k: Nat,
                      dataType: DataType,
                      fragmentType: FragmentType,
                      matrix: Phrase[ExpType],
                      layout: MatrixLayout) extends ExpPrimitive {

  private val fragType = Fragment(m, n, k, dataType, fragmentType, layout)
  matrix :: ExpType(fragType.matrixType, write)
  override val t: ExpType = ExpType(fragType, write)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    ToFragment(f.nat(m), f.nat(n), f.nat(k), f.data(dataType), fragmentType,
      VisitAndRebuild(matrix, f), layout)
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    con(matrix)(λ(ExpType(fragType.matrixType, read))(matrix =>
      WmmaLoad(m, n, k, dataType, matrix, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = ???

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"toFragment($m, $n, $k, $dataType, $fragmentType, ${PrettyPhrasePrinter(matrix)})"

  override def xmlPrinter: Elem =
    <ToFragment m={ToString(m)} n={ToString(n)} k={ToString(k)}>
      <matrixTile>
        {Phrases.xmlPrinter(matrix)}
      </matrixTile>
    </ToFragment>
}
