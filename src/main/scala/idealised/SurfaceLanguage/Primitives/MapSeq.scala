package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

//noinspection TypeAnnotation
final case class MapSeq(f: Expr[DataType -> DataType], array: DataExpr,
                        override val t: Option[DataType] = None)
  extends AbstractMap(f, array, t) {

  override def makeDPIAMap = DPIA.FunctionalPrimitives.MapSeq

  override def makeMap = MapSeq
}
