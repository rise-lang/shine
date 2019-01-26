package idealised.OpenMP.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Primitives.AbstractMap
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types.DataType

//noinspection TypeAnnotation
final case class MapPar(f: Expr[DataType -> DataType], array: DataExpr,
                        override val t: Option[DataType] = None)
  extends AbstractMap(f, array, t)
{
  override def makeMap = MapPar

  override def makeDPIAMap = idealised.OpenMP.FunctionalPrimitives.MapPar
}
