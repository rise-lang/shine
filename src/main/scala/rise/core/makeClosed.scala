package rise.core

import scala.collection.immutable.Map
import rise.core.types._

object makeClosed {
  def apply(e: Expr): Expr = withCount(e)._1
  def withCount(e: Expr): (Expr, Int) = {
    val emptySubs: (Map[Type, Type],
      Map[NatIdentifier, Nat],
      Map[AddressSpaceIdentifier, AddressSpace],
      Map[NatToDataIdentifier, NatToData]) = (Map(), Map(), Map(), Map())

    val (expr, (ts, ns, as, n2ds)) = DSL.infer.getFTVsRec(e).foldLeft((e, emptySubs))((acc, ftv) => acc match {
      case (expr, (ts, ns, as, n2ds)) => ftv match {
        case i: TypeIdentifier =>
          val dt = DataTypeIdentifier(freshName("dt"), isExplicit = true)
          (DepLambda(DataKind, dt, expr)(DepFunType(DataKind, dt, expr.t)),
            (ts ++ Map(i -> dt), ns, as , n2ds))
        case i: DataTypeIdentifier =>
          val dt = i.asExplicit
          (DepLambda(DataKind, dt, expr)(DepFunType(DataKind, dt, expr.t)),
            (ts ++ Map(i -> dt), ns, as , n2ds))
        case i: NatIdentifier =>
          val n = i.asExplicit
          (DepLambda(NatKind, n, expr)(DepFunType(NatKind, n, expr.t)),
            (ts, ns ++ Map(i -> n), as, n2ds))
        case i: AddressSpaceIdentifier =>
          val a = i.asExplicit
          (DepLambda(AddressSpaceKind, a, expr)(DepFunType(AddressSpaceKind, a, expr.t)),
            (ts, ns, as ++ Map(i -> a), n2ds))
        case i: NatToDataIdentifier =>
          val n2d = i.asExplicit
          (DepLambda(NatToDataKind, n2d, expr)(DepFunType(NatToDataKind, n2d, expr.t)),
            (ts, ns, as, n2ds ++ Map(i -> n2d)))
        case i => throw TypeException(s"${i.getClass} is not supported yet")
      }
    })
    (new Solution(ts, ns, as, Map.empty, Map.empty, n2ds, Map.empty, Map.empty)(expr), ts.size + ns.size + as.size + n2ds.size)
  }
}
