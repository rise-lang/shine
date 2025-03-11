package rise.core.DSL

import Type.impl
import util.monads._
import rise.core.traverse._
import rise.core.types._
import rise.core.{DSL, Expr, IsClosedForm, Primitive}

final case class TopLevel(e: Expr, inst: Solution = Solution())(
  override val t: ExprType = e.t
) extends Primitive {
  import DSL.TopLevel._
  // TODO: Ignored by alpha equivalence, remove when taking out of primitives
  override def primEq(obj: Primitive): Boolean = obj.getClass == getClass
  override def typeScheme: ExprType = e.t
  override def setType(t: ExprType): TopLevel = {
    val subs = instantiate(t)
    this.copy(inst = subs)(subs(t))
  }
  override def name: String = s"TopLevel Expr: $t"
}

object TopLevel {
  private def instantiate(t: ExprType): Solution = {
    import scala.collection.immutable.Map
    IsClosedForm.varsToClose(t).foldLeft(Solution())((subs, ftv) =>
      subs match {
        case s@Solution(ts, ns, as, ms, fs, n2ds, n2ns, natColls) =>
          ftv match {
            case TypeKind.IDWrapper(i) =>
              s.copy(ts = ts ++ Map(i -> impl{ x: TypeIdentifier => x }))
            case DataKind.IDWrapper(i) =>
              s.copy(ts = ts ++ Map(i -> impl{ x: DataType => x }))
            case NatKind.IDWrapper(i) =>
              s.copy(ns = ns ++ Map(i -> impl{ x: Nat => x }))
            case AddressSpaceKind.IDWrapper(i) =>
              s.copy(as = as ++ Map(i -> impl{ x: AddressSpace => x }))
            case MatrixLayoutKind.IDWrapper(i) =>
              s.copy(ms = ms ++ Map(i -> impl{ x: MatrixLayout => x }))
            case FragmentKind.IDWrapper(i) =>
              s.copy(fs = fs ++ Map(i -> impl{ x: Fragment => x }))
            case NatToDataKind.IDWrapper(i) =>
              s.copy(n2ds = n2ds ++ Map(i -> impl{ x: NatToData => x }))
            case NatToNatKind.IDWrapper(i) =>
              s.copy(n2ns = n2ns ++ Map(i -> impl{ x: NatToNat => x }))
            case NatCollectionKind.IDWrapper(i) =>
              s.copy(natColls = natColls ++ Map(i -> impl{ x: NatCollection => x }))
            case AccessKind.IDWrapper(_) => ???
          }
      }
    )
  }

  case class Visitor(ftvSubs: Solution, sol: Solution) extends PureTraversal {
    override def nat : Nat => Pure[Nat] = n => return_(cascadedApply(ftvSubs, sol, n))
    override def `type`[T <: ExprType] : T => Pure[T] = t => return_(cascadedApply(ftvSubs, sol, t).asInstanceOf[T])
    override def addressSpace : AddressSpace => Pure[AddressSpace] = a => return_(cascadedApply(ftvSubs, sol, a))
    override def natToData : NatToData => Pure[NatToData] = n2d => return_(cascadedApply(ftvSubs, sol, n2d))
  }

  private def cascadedApply(ftvSubs: Solution, sol: Solution, t: ExprType): ExprType = {
    traverse(t, new Visitor(ftvSubs, sol) {
        override def `type`[T <: ExprType] : T => Pure[T] = {
          case i: TypeIdentifier =>
            this.ftvSubs.ts.get(i) match {
              case None => super.`type`(i.asInstanceOf[T])
              case Some(j) =>
                this.sol.ts.get(j) match {
                  case Some(x) => return_(x.asInstanceOf[T])
                  case None    => super.`type`(i.asInstanceOf[T])
                }
            }
          case t => super.`type`(t)
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