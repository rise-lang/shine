package idealised.OpenMP.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Primitives.AbstractMap
import idealised.SurfaceLanguage.Types.DataType
import idealised.SurfaceLanguage._

//noinspection TypeAnnotation
final case class MapPar(f: Expr, array: Expr,
                        override val t: Option[DataType] = None)
  extends AbstractMap(f, array, t)
{
  override def makeMap = MapPar

  override def makeDPIAMap = idealised.OpenMP.FunctionalPrimitives.MapPar
}
