package rise.core

import scala.collection.immutable.Map
import rise.core.types._

object makeClosed {
  def apply(e: Expr): Expr = withCount(e)._1
  def withCount(e: Expr): (Expr, Int) = {
    val (expr, ts) = IsClosedForm.varsToClose(e)._2.foldLeft((e, Map[Type, Type]()))((acc, ftv) => acc match {
      case (expr, ts) => ftv match {
        case i: TypeIdentifier =>
          val dt = DataTypeIdentifier(freshName("dt"), isExplicit = true)
          (DepLambda[DataKind](dt, expr)(DepFunType[DataKind, Type](dt, expr.t)), (ts ++ Map(i -> dt)))
        case i: DataTypeIdentifier =>
          (DepLambda[DataKind](i, expr)(DepFunType[DataKind, Type](i, expr.t)), ts)
        case i: NatIdentifier =>
          (DepLambda[NatKind](i, expr)(DepFunType[NatKind, Type](i, expr.t)), ts)
        case i: AddressSpaceIdentifier =>
          (DepLambda[AddressSpaceKind](i, expr)(DepFunType[AddressSpaceKind, Type](i, expr.t)), ts)
        case i: NatToDataIdentifier =>
          (DepLambda[NatToDataKind](i, expr)(DepFunType[NatToDataKind, Type](i, expr.t)), ts)
        case i => throw TypeException(s"${i.getClass} is not supported yet")
      }
    })
    (new Solution(ts, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)(expr), ts.size)
  }
}
