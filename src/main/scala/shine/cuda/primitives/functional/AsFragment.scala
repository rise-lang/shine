package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.con
import shine.DPIA.DSL._
import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.cuda.primitives.imperative.WmmaLoad
import shine.macros.Primitive.expPrimitive

object AsFragment{
  def apply(rows: Nat, columns: Nat, d3: Nat, dataType: DataType, fragmentType: FragmentKind, matrix: Phrase[ExpType]):
  AsFragment = AsFragment(rows, columns, d3, dataType, fragmentType, matrix, MatrixLayoutIdentifier("ml"))
}

@expPrimitive
case class AsFragment(rows: Nat,
                      columns: Nat,
                      d3: Nat,
                      dataType: DataType,
                      fragmentType: FragmentKind,
                      matrix: Phrase[ExpType],
                      layout: MatrixLayout) extends ExpPrimitive with AccT {

  matrix :: ExpType(ArrayType(rows, ArrayType(columns, dataType)), write)
  override val t: ExpType = ExpType(FragmentType(rows, columns, d3, dataType, fragmentType, layout), write)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    con(matrix)(λ(ExpType(ArrayType(rows, ArrayType(columns, dataType)), read))(matrix =>
      WmmaLoad(rows, columns, d3, dataType, fragmentType, layout, matrix, A)))
  }
}
