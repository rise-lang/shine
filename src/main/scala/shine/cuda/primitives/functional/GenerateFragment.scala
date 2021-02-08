package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.con
import shine.DPIA.DSL._
import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.cuda.primitives.imperative.WmmaFill
import shine.macros.Primitive.expPrimitive

@expPrimitive
case class GenerateFragment(rows: Nat,
                            columns: Nat,
                            d3: Nat,
                            dataType: DataType,
                            fill: Phrase[ExpType],
                            fragmentType: FragmentKind,
                            layout: MatrixLayout) extends ExpPrimitive with AccT {

  fill :: ExpType(dataType, read)

  override val t: ExpType = ExpType(FragmentType(rows, columns, d3, dataType), write)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    con(fill)(λ(ExpType(dataType, read))(fill =>
      WmmaFill(rows, columns, d3, dataType, fill, fragmentType, layout, A)))
  }
}
