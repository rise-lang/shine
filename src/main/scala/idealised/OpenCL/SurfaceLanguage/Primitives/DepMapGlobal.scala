package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Primitives.AbstractDepMap
import idealised.SurfaceLanguage.Types.DataType

//noinspection TypeAnnotation
final case class DepMapGlobal(dim:Int)(f: Expr, array: Expr,
                                       override val t: Option[DataType] = None)
  extends AbstractDepMap(f, array, t)
{
  override def makeMap = DepMapGlobal(dim)

  override def makeDPIAMap = idealised.OpenCL.FunctionalPrimitives.DepMapGlobal(dim)
}

