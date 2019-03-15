package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Primitives.AbstractDepMap
import idealised.SurfaceLanguage.Types.DataType

final case class DepMapLocal(dim: Int)(f: Expr, array: Expr,
                                       override val t: Option[DataType] = None)
  extends AbstractDepMap(f, array, t) {
  override def makeMap = DepMapLocal(dim) _
}
