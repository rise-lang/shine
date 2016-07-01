package ExpPatterns

import CommandPatterns.MapSeqI
import Core._
import apart.arithmetic.ArithExpr

case class MapSeq(n: ArithExpr,
                  dt1: DataType,
                  dt2: DataType,
                  f: Phrase[ExpType -> ExpType],
                  array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array, MapSeq, MapSeqI)
