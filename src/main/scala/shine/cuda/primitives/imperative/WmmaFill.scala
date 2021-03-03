package shine.cuda.primitives.imperative

import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.macros.Primitive.comPrimitive

/**
  * Fills a fragment with a specific value. <br>
  * This primitive needs to be executed by a full warp!
  * @param rows         number of rows of the fragment ({@link FragmentType#rows})
  * @param columns      number of columns of the fragment ({@link FragmentType#columns})
  * @param d3           third dimension which is used in the MMA operation ({@link FragmentType#d3})
  * @param dataType     dataType of the elements in the fragment ({@link FragmentType#datatype})
  * @param fill         new value of all elements in the fragment (type of fill: `dataType`)
  * @param fragmentKind kind of the fragment ({@link FragmentType#fragmentKind})
  * @param layout       layout of the fragment ({@link FragmentType#layout})
  * @param fragment     fragment-Acceptor whose elements should be changed to `fill`
  */
@comPrimitive
case class WmmaFill(rows: Nat,
                    columns: Nat,
                    d3: Nat,
                    dataType: DataType,
                    fill: Phrase[ExpType],
                    fragmentKind: FragmentKind,
                    layout: MatrixLayout,
                    fragment: Phrase[AccType]
                   ) extends CommandPrimitive {
  fill :: ExpType(dataType, read)
  fragment :: AccType(FragmentType(rows, columns, d3, dataType, fragmentKind, layout))

  override def prettyPrint: String =
    s"WmmaFill(${PrettyPhrasePrinter(fill)}, ${PrettyPhrasePrinter(fragment)})"
}
