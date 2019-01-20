package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

final case class ReduceSeq(f: Expr[DataType -> (DataType -> DataType)],
                           init: DataExpr,
                           array: DataExpr,
                           override val t: Option[DataType])
  extends AbstractReduce(f, init, array, t) {

  override def makeDPIAReduce = DPIA.FunctionalPrimitives.ReduceSeq

  override def makeReduce = ReduceSeq

}
