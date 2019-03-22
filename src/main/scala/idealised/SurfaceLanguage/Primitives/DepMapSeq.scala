package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

//noinspection TypeAnnotation
final case class DepMapSeq(override val df: Expr, override val array: Expr,
                           override val t: Option[DataType])
  extends AbstractDepMap(df, array, t) {

  override def makeMap = DepMapSeq
}


//noinspection TypeAnnotation
final case class DepMapSeqUnroll(override val df: Expr, override val array: Expr,
                                 override val t: Option[DataType])
  extends AbstractDepMap(df, array, t) {

  override def makeMap = DepMapSeqUnroll
}
