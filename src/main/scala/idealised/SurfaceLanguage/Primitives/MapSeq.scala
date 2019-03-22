package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

//noinspection TypeAnnotation
final case class MapSeq(override val f: Expr, override val array: Expr,
                        override val t: Option[DataType] = None)
  extends AbstractMap(f, array, t) {

  override def makeMap = MapSeq
}
