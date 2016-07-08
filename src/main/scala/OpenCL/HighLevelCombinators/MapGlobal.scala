package OpenCL.HighLevelCombinators

import Core._
import HighLevelCombinators.AbstractMap
import OpenCL.MidLevelCombinators.MapGlobalI
import apart.arithmetic.ArithExpr

case class MapGlobal(n: ArithExpr,
                     dt1: DataType,
                     dt2: DataType,
                     f: Phrase[ExpType -> ExpType],
                     array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array, MapGlobal, MapGlobalI)
