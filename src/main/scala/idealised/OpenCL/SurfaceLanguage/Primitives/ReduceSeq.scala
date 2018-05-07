package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.OpenCL
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Primitives.AbstractReduce

final case class ReduceSeq(f: Expr[DataType -> (DataType -> DataType)],
                           init: DataExpr, array: DataExpr,
                           override val t: Option[DataType] = None)
  extends AbstractReduce(f, init, array, t)
{
  override def makeReduce: (Expr[DataType -> (DataType -> DataType)], DataExpr, DataExpr, Option[DataType]) =>
    AbstractReduce = ReduceSeq

  override def makeDPIAReduce = OpenCL.FunctionalPrimitives.ReduceSeq
}
