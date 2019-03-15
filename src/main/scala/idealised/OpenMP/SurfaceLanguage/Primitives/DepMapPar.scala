package idealised.OpenMP.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Primitives.AbstractDepMap
import idealised.SurfaceLanguage.Types.DataType
import idealised.SurfaceLanguage._

//noinspection TypeAnnotation
final case class DepMapPar(override val df: Expr, override val array: Expr,
                           override val t: Option[DataType] = None)
  extends AbstractDepMap(df, array, t)
{
  override def makeMap = DepMapPar
}
