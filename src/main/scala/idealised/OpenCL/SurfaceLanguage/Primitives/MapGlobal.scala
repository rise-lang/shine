package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Primitives.AbstractMap
import idealised.SurfaceLanguage.Types._

final case class MapGlobal(dim: Int)(f: Expr[DataType -> DataType],
                                     array: DataExpr,
                                     override val `type`: Option[DataType] = None)
  extends AbstractMap(f, array, `type`)
{
  override def makeMap = MapGlobal(dim)

  override def makeDPIAMap = idealised.OpenCL.FunctionalPrimitives.MapGlobal(dim)
}
