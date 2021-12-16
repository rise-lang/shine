package rise.core.primitives

import rise.core.DSL.ToBeTyped
import rise.core.DSL.Type._
import rise.core._
import rise.core.types._

object depSlide extends Builder {
  private final case class Primitive()
                                    (override val t: ExprType = TypePlaceholder)
    extends rise.core.Primitive
  {
    override val name: String = "depSlide"
    override def primEq(obj: rise.core.Primitive): Boolean = obj.getClass == getClass
    override def setType(ty: ExprType): Primitive = Primitive()(ty)
    override def typeScheme: ExprType =
      expl{ n: Nat => expl((sz: Nat) => expl((sp: Nat) => impl{ t: DataType =>
        import arithexpr.arithmetic.BoolExpr.ArithPredicate.Operator
        import arithexpr.arithmetic.BoolExpr.arithPredicate
        import arithexpr.arithmetic.IfThenElse

        val halo = sz - sp
        val allWindows = (n - halo + sp - 1) / sp
        val fullWindows = (n - halo) / sp
        val remainder = (n - halo) % sp
        (n `.` t) ->: (allWindows`*.`(i =>
          (IfThenElse(arithPredicate(i, fullWindows, Operator.<), sp, remainder) + halo) `.` t))
      }))}
  }

  override def primitive: rise.core.Primitive = Primitive()()
  override def apply: ToBeTyped[rise.core.Primitive] = DSL.toBeTyped(Primitive()())
  override def unapply(arg: Expr): Boolean = arg match {
    case _: Primitive => true
    case _ => false
  }
}
