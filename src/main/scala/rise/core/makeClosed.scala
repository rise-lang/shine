package rise.core

import rise.core.types.Kind.{IAddressSpace, IDataType, INat, INatToData, INatToNat, IType}

import scala.collection.immutable.Map
import rise.core.types._

object makeClosed {
  def apply(e: Expr): Expr = withCount(e)._1
  def withCount(e: Expr): (Expr, Int) = {
    val (_, ftvs) = IsClosedForm.varsToClose(e)
    val (expr, ts) = ftvs.foldLeft((e, Map[Type, Type]()))((acc, ftv) => acc match {
      case (expr, ts) => ftv match {
        case IType(i) =>
          val dt = DataTypeIdentifier(freshName("dt"))
          (DepLambda(DataKind, dt, expr)(DepFunType(DataKind, dt, expr.t)), (ts ++ Map(i -> dt)))
        case IDataType(i) =>
          (DepLambda(DataKind, i, expr)(DepFunType(DataKind, i, expr.t)), ts)
        case INat(i) =>
          (DepLambda(NatKind, i, expr)(DepFunType(NatKind, i, expr.t)), ts)
        case IAddressSpace(i) =>
          (DepLambda(AddressSpaceKind, i, expr)(DepFunType(AddressSpaceKind, i, expr.t)), ts)
        case INatToData(i) =>
          (DepLambda(NatToDataKind, i, expr)(DepFunType(NatToDataKind, i, expr.t)), ts)
        case i => throw TypeException(s"${i.getClass} is not supported yet")
      }
    })
    val sol = new Solution(ts, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
    (sol(expr), ftvs.length)
  }
}
