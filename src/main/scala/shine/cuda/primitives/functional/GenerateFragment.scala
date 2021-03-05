package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.con
import shine.DPIA.DSL._
import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.cuda.primitives.imperative.WmmaFill
import shine.macros.Primitive.expPrimitive

/**
  * Returns a fragment in which all elements have a specific value ({@link WmmaFill}). <br>
  * This primitive needs to be executed by a full warp!
  * @param rows         number of rows of the fragment ({@link FragmentType#rows})
  * @param columns      number of columns of the fragment ({@link FragmentType#columns})
  * @param d3           third dimension which is used in the MMA operation ({@link FragmentType#d3})
  * @param dataType     dataType of the elements in the fragment ({@link FragmentType#datatype})
  * @param fill         new value of all elements in the fragment (type of fill: `dataType`)
  * @param fragmentKind kind of the fragment ({@link FragmentType#fragmentKind})
  * @param layout       layout of the fragment ({@link FragmentType#layout})
  */
@expPrimitive
case class GenerateFragment(rows: Nat,
                            columns: Nat,
                            d3: Nat,
                            dataType: DataType,
                            fill: Phrase[ExpType],
                            fragmentKind: FragmentKind,
                            layout: MatrixLayout) extends ExpPrimitive with AccT {

  fill :: ExpType(dataType, read)

  override val t: ExpType = ExpType(FragmentType(rows, columns, d3, dataType), write)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    con(fill)(λ(ExpType(dataType, read))(fill =>
      WmmaFill(rows, columns, d3, dataType, fill, fragmentKind, layout, A)))
  }
}
