package idealised.OpenMP.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Primitives.AbstractMap
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types.DataType

final case class MapSeq(f: Expr[DataType -> DataType], array: DataExpr,
                        override val t: Option[DataType] = None)
  extends AbstractMap(f, array, t)
{
  override def makeMap = MapSeq

  override def makeDPIAMap = idealised.OpenMP.FunctionalPrimitives.MapSeq
}
