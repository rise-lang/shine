package idealised.OpenMP.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Primitives.AbstractReduce
import idealised.SurfaceLanguage.Types.DataType

//noinspection TypeAnnotation
final case class ReducePar(override val f: Expr,
                           override val init: Expr, override val array: Expr,
                           override val t: Option[DataType] = None)
  extends AbstractReduce(f, init, array, t)
{
  override def makeReduce = ReducePar
}
