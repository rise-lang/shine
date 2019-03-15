package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

//noinspection TypeAnnotation
final case class ScanSeq(override val f: Expr, override val init: Expr, override val array: Expr,
                         override val t: Option[DataType])
  extends AbstractScan(f, init, array, t) {

  override def makeScan = ScanSeq
}
