package shine.cuda.primitives.functional

import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.macros.Primitive.expPrimitive

/**
  * Returns a matrix tile with the elements from the `Accumulator`-fragment. The matrix tile must resides
  * in shared or global memory ({@link WmmaStore}). <br>
  * This primitive needs to be executed by a full warp!
  * @param rows       number of rows of the fragment ({@link FragmentType#rows})
  * @param columns    number of columns of the fragment ({@link FragmentType#columns})
  * @param d3         third dimension which is used in the MMA operation ({@link FragmentType#d3})
  * @param dataType   dataType of the elements in the fragment ({@link FragmentType#datatype})
  * @param fragment   fragment from which the elements should be stored
  */
@expPrimitive
case class AsMatrix(rows: Nat,
                    columns: Nat,
                    d3: Nat,
                    dataType: DataType,
                    fragment: Phrase[ExpType]) extends ExpPrimitive {

  fragment :: ExpType(FragmentType(rows, columns, d3, dataType), read)
  override val t: ExpType = ExpType(ArrayType(rows, ArrayType(columns, dataType)), write)
}
