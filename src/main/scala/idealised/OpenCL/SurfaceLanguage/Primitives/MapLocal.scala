package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Primitives.AbstractMap

final case class MapLocal(dim: Int)(f: Expr,
                                    array: Expr,
                                    override val t: Option[DataType] = None)
  extends AbstractMap(f, array, t)
{
  override def makeMap = MapLocal(dim)

  override def makeDPIAMap = idealised.OpenCL.FunctionalPrimitives.MapLocal(dim)
}

