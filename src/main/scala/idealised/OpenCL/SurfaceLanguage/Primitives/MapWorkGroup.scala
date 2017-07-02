package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Primitives.AbstractMap

final case class MapWorkGroup(dim: Int)(f: Expr[DataType -> DataType],
                                        array: DataExpr,
                                        override val `type`: Option[DataType] = None)
  extends AbstractMap(f, array, `type`)
{
  override def makeMap = MapWorkGroup(dim)

  override def makeDPIAMap = idealised.OpenCL.FunctionalPrimitives.MapWorkGroup(dim)
}
