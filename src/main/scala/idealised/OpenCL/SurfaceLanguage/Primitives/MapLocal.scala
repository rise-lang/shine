package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.OpenCL.AddressSpace
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Primitives.AbstractMap
import idealised.SurfaceLanguage.Types._

final case class MapLocal(dim: Int, addressSpace: AddressSpace)(f: Expr,
                                    array: Expr,
                                    override val t: Option[DataType] = None)
  extends AbstractMap(f, array, t) {
  override def makeMap = MapLocal(dim, addressSpace)
}

