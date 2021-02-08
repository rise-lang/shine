package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.con
import shine.DPIA.DSL._
import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.cuda.primitives.imperative.WmmaStore
import shine.macros.Primitive.expPrimitive

@expPrimitive
case class AsMatrix(rows: Nat,
                    columns: Nat,
                    d3: Nat,
                    dataType: DataType,
                    fragment: Phrase[ExpType]) extends ExpPrimitive with AccT {

  fragment :: ExpType(FragmentType(rows, columns, d3, dataType), read)
  override val t: ExpType = ExpType(ArrayType(rows, ArrayType(columns, dataType)), write)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    con(fragment)(λ(ExpType(fragment.t.dataType, read))(fragment =>
      WmmaStore(rows, columns, d3,
        dataType, fragment, A)))
  }
}
