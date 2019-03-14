package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Primitives.AbstractMap
import idealised.SurfaceLanguage.Types._

final case class MapGlobal(dim: Int)(f: Expr,
                                     array: Expr,
                                     override val t: Option[DataType] = None)
  extends AbstractMap(f, array, t)
{
  override def makeMap = MapGlobal(dim)

  override def makeDPIAMap = idealised.OpenCL.FunctionalPrimitives.MapGlobal(dim)
}
