package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Primitives.AbstractMap

final case class MapWorkGroup(f: Expr[DataType -> DataType], array: DataExpr,
                              override val `type`: Option[DataType] = None)
  extends AbstractMap(f, array, `type`)
{
  override def makeMap: (Expr[DataType -> DataType], DataExpr, Option[DataType]) => AbstractMap = MapWorkGroup

  override def makeDPIAMap = idealised.OpenCL.FunctionalPrimitives.MapWorkGroup
}
