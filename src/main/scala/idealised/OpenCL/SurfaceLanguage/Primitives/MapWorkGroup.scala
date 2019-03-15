package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Primitives.AbstractMap
import idealised.SurfaceLanguage.Types._

final case class MapWorkGroup(dim: Int)(f: Expr,
                                        array: Expr,
                                        override val t: Option[DataType] = None)
  extends AbstractMap(f, array, t) {
  override def makeMap = MapWorkGroup(dim)
}
