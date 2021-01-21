package shine.DPIA.Compilation

import scala.language.implicitConversions

import shine.DPIA.Nat

sealed trait PathExpr
sealed trait PairAccess extends PathExpr
case object FstMember extends PairAccess
case object SndMember extends PairAccess
final case class CIntExpr(num: Nat) extends PathExpr
case object DPairSnd extends PathExpr

object PathExpr {
  implicit def cIntExprToNat(cexpr: CIntExpr): Nat = cexpr.num
}
