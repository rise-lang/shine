package idealised.OpenMP.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{->, Expr}
import idealised.SurfaceLanguage.Primitives.AbstractReduce
import idealised.SurfaceLanguage.Types.DataType

final case class ReduceSeq(f: Expr[DataType -> (DataType -> DataType)],
                           init: DataExpr, array: DataExpr,
                           override val t: Option[DataType] = None)
  extends AbstractReduce(f, init, array, t)
{
  override def makeReduce = ReduceSeq

  override def makeDPIAReduce = idealised.OpenMP.FunctionalPrimitives.ReduceSeq
}
