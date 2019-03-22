package idealised.OpenMP.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Primitives.AbstractMap
import idealised.SurfaceLanguage.Types.DataType
import idealised.SurfaceLanguage._

//noinspection TypeAnnotation
final case class MapPar(override val f: Expr, override val array: Expr,
                        override val t: Option[DataType] = None)
  extends AbstractMap(f, array, t)
{
  override def makeMap = MapPar
}
