package idealised.OpenMP.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Primitives.AbstractDepMap
import idealised.SurfaceLanguage.Types.DataType
import idealised.SurfaceLanguage._

//noinspection TypeAnnotation
final case class DepMapPar(f: Expr, array: Expr,
                           override val t: Option[DataType] = None)
  extends AbstractDepMap(f, array, t)
{
  override def makeMap = DepMapPar

  override def makeDPIAMap = idealised.OpenMP.FunctionalPrimitives.DepMapPar
}
