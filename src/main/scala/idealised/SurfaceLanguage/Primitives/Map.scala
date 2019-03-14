package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

//noinspection TypeAnnotation
final case class Map(f: Expr, array: Expr,
                     override val t: Option[DataType] = None)
  extends AbstractMap(f, array, t) {

  override def makeDPIAMap = DPIA.FunctionalPrimitives.Map

  override def makeMap = Map
}
