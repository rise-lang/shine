package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

//noinspection TypeAnnotation
final case class ScanSeq(f: Expr, init:Expr, array: Expr,
                         override val t: Option[DataType])
  extends AbstractScan(f, init, array, t) {

  override def makeDPIAScan = DPIA.FunctionalPrimitives.ScanSeq

  override def makeScan = ScanSeq
}
