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
  def apply(rows: Nat, columns: Nat, d3: Nat, dataType: DataType, fragmentType: FragmentType, matrix: Phrase[ExpType]):
  ToFragment = ToFragment(rows, columns, d3, dataType, fragmentType, matrix, MatrixLayoutIdentifier("ml"))
}

case class ToFragment(rows: Nat,
                      columns: Nat,
                      d3: Nat,
                      dataType: DataType,
                      fragmentType: FragmentType,
                      matrix: Phrase[ExpType],
                      layout: MatrixLayout) extends ExpPrimitive {

  matrix :: ExpType(ArrayType(rows, ArrayType(columns, dataType)), write)
  override val t: ExpType = ExpType(Fragment(rows, columns, d3, dataType, fragmentType, layout), write)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    ToFragment(f.nat(rows), f.nat(columns), f.nat(d3), f.data(dataType), fragmentType, VisitAndRebuild(matrix, f), layout)
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    con(matrix)(λ(ExpType(ArrayType(rows, ArrayType(columns, dataType)), read))(matrix =>
      WmmaLoad(rows, columns, d3, dataType, fragmentType, layout, matrix, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = ???

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"toFragment($rows, $columns, $d3, $dataType, $fragmentType, ${PrettyPhrasePrinter(matrix)})"

  override def xmlPrinter: Elem =
    <ToFragment m={ToString(rows)} n={ToString(columns)} k={ToString(d3)}>
      <matrixTile>
        {Phrases.xmlPrinter(matrix)}
      </matrixTile>
    </ToFragment>
}
