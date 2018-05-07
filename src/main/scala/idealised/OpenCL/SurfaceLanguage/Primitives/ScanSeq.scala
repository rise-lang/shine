package idealised.OpenCL.SurfaceLanguage.Primitives

/**
  * Created by federico on 13/01/18.
  */
import idealised.OpenCL
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Primitives.AbstractScan
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, _}

final case class ScanSeq(f: Expr[DataType -> (DataType -> DataType)],
                           init: DataExpr, array: DataExpr,
                           override val `type`: Option[DataType] = None)
  extends AbstractScan(f, init, array, `type`)
{

  override def makeScan: (Expr[DataType -> (DataType -> DataType)], DataExpr, DataExpr, Option[DataType]) =>
    AbstractScan = ScanSeq

  override def makeDPIAScan = OpenCL.FunctionalPrimitives.ScanSeq
}
