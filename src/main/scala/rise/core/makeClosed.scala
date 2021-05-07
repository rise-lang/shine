package rise.core

import scala.collection.immutable.Map
import rise.core.types._

object makeClosed {
  def apply(e: Expr): Expr = withCount(e)._1
  def withCount(e: Expr): (Expr, Int) = {
    val (expr, ts) = DSL.infer.getFTVsRec(e).foldLeft((e, Map[Type, Type]()))((acc, ftv) => acc match {
      case (expr, ts) => ftv match {
        case i: TypeIdentifier =>
          val dt = DataTypeIdentifier(freshName("dt"))
          (DepLambda[DataKind](dt, expr)(DepFunType[DataKind, Type](dt, expr.t)), ts ++ Map(i -> dt))
        case dt: DataTypeIdentifier =>
          (DepLambda[DataKind](dt, expr)(DepFunType[DataKind, Type](dt, expr.t)), ts)
        case n: NatIdentifier =>
          (DepLambda[NatKind](n, expr)(DepFunType[NatKind, Type](n, expr.t)), ts)
        case a: AddressSpaceIdentifier =>
          (DepLambda[AddressSpaceKind](a, expr)(DepFunType[AddressSpaceKind, Type](a, expr.t)), ts)
        case n2d: NatToDataIdentifier =>
          (DepLambda[NatToDataKind](n2d, expr)(DepFunType[NatToDataKind, Type](n2d, expr.t)), ts)
        case i => throw TypeException(s"${i.getClass} is not supported yet")
      }
    })
    (new Solution(ts, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)(expr), ts.size)
  }
}
