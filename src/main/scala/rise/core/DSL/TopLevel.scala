package rise.core.DSL

import Type.impl
import rise.core.traversal.{Continue, Result, Stop}
import rise.core.types._
import rise.core.{DSL, Expr, Primitive, traversal}

final case class TopLevel(e: Expr, inst: Solution = Solution())(
  override val t: Type = e.t
) extends Primitive {
  import DSL.TopLevel._
  override def typeScheme: Type = e.t
  override def setType(t: Type): TopLevel = {
    val subs = instantiate(t)
    this.copy(inst = subs)(subs(t))
  }
  override def name: String = s"TopLevel Expr: $t"
}

object TopLevel {
  private def instantiate(t: Type): Solution = {
    import scala.collection.immutable.Map
    infer.getFTVs(t).foldLeft(Solution())((subs, ftv) =>
      subs match {
        case s@Solution(ts, ns, as, ms, fs, n2ds, n2ns, natColls) =>
          ftv match {
            case i: TypeIdentifier =>
              s.copy(ts = ts ++ Map(i -> impl{ x: TypeIdentifier => x }))
            case i: DataTypeIdentifier =>
              s.copy(ts = ts ++ Map(i -> impl{ x: DataType => x }))
            case i: NatIdentifier =>
              s.copy(ns = ns ++ Map(i -> impl{ x: Nat => x }))
            case i: AddressSpaceIdentifier =>
              s.copy(as = as ++ Map(i -> impl{ x: AddressSpace => x }))
            case i: MatrixLayoutIdentifier =>
              s.copy(ms = ms ++ Map(i -> impl{ x: MatrixLayout => x }))
            case i: FragmentTypeIdentifier =>
              s.copy(fs = fs ++ Map(i -> impl{ x: FragmentType => x }))
            case i: NatToDataIdentifier =>
              s.copy(n2ds = n2ds ++ Map(i -> impl{ x: NatToData => x }))
            case i: NatToNatIdentifier =>
              s.copy(n2ns = n2ns ++ Map(i -> impl{ x: NatToNat => x }))
            case i: NatCollectionIdentifier =>
              s.copy(natColls = natColls ++ Map(i -> impl{ x: NatCollection => x }))
            case i =>
              throw TypeException(s"${i.getClass} is not supported yet")
          }
      }
    )
  }

  case class Visitor(ftvSubs: Solution, sol: Solution)
    extends traversal.Visitor {
    override def visitExpr(e: Expr): Result[Expr] = Continue(e, this)
    override def visitNat(ae: Nat): Result[Nat] =
      Stop(cascadedApply(ftvSubs, sol, ae))
    override def visitType[T <: Type](t: T): Result[T] =
      Stop(cascadedApply(ftvSubs, sol, t).asInstanceOf[T])
    override def visitAddressSpace(a: AddressSpace): Result[AddressSpace] =
      Stop(cascadedApply(ftvSubs, sol, a))
    override def visitN2D(n2d: NatToData): Result[NatToData] =
      Stop(cascadedApply(ftvSubs, sol, n2d))
  }

  private def cascadedApply(
                             ftvSubs: Solution,
                             sol: Solution,
                             t: Type
                           ): Type = {
    traversal.types.DepthFirstLocalResult(
      t,
      new Visitor(ftvSubs, sol) {
        override def visitType[T <: Type](t: T): Result[T] = t match {
          case i: TypeIdentifier =>
            ftvSubs.ts.get(i) match {
              case Some(i) =>
                sol.ts.get(i.asInstanceOf[TypeIdentifier]) match {
                  case Some(x) => Stop(x.asInstanceOf[T])
                  case None    => Continue(t, this)
                }
              case None => Continue(t, this)
            }
          case _ => Continue(t, this)
        }
      }
    )
  }

  private def cascadedApply(ftvSubs: Solution, sol: Solution, n: Nat): Nat = {
    n.visitAndRebuild {
      case i: NatIdentifier =>
        ftvSubs.ns.get(i) match {
          case Some(n) =>
            sol.ns.get(n.asInstanceOf[NatIdentifier]) match {
              case Some(x) => x
              case None    => n
            }
          case None => i
        }
      case n => n
    }
  }

  private def cascadedApply(
                             ftvSubs: Solution,
                             sol: Solution,
                             a: AddressSpace
                           ): AddressSpace = {
    a match {
      case i: AddressSpaceIdentifier =>
        ftvSubs.as.get(i) match {
          case Some(a) =>
            sol.as.get(a.asInstanceOf[AddressSpaceIdentifier]) match {
              case Some(x) => x
              case None    => a
            }
          case None => i
        }
      case a => a
    }
  }

  private def cascadedApply(
                             ftvSubs: Solution,
                             sol: Solution,
                             n2d: NatToData
                           ): NatToData = {
    n2d match {
      case i: NatToDataIdentifier =>
        ftvSubs.n2ds.get(i) match {
          case Some(n2d) =>
            sol.n2ds.get(n2d.asInstanceOf[NatToDataIdentifier]) match {
              case Some(x) => x
              case None    => n2d
            }
          case None => i
        }
      case n2d => n2d
    }
  }
}