package rise.core.primitives

import rise.core.DSL.ToBeTyped
import rise.core.DSL.Type._
import rise.core._
import rise.core.types._

object depTile extends Builder {
  private final case class Primitive()
                                    (override val t: Type = TypePlaceholder)
    extends rise.core.Primitive
  {
    override val name: String = "depTile"
    override def setType(ty: Type): Primitive = Primitive()(ty)
    override def typeScheme: Type =
      impl{ n: Nat => expl((tile: Nat) => impl{ halo: Nat =>
        impl{ s: DataType => impl{ t: DataType =>
          import arithexpr.arithmetic.BoolExpr.ArithPredicate.Operator
          import arithexpr.arithmetic.BoolExpr.arithPredicate
          import arithexpr.arithmetic.IfThenElse

          val allTiles = (n + tile - 1) / tile
          val fullTiles = n / tile
          val remainder = n % tile
          def depSize(i: Nat) =
            IfThenElse(arithPredicate(i, fullTiles, Operator.<), tile, remainder)
          ((allTiles `*.` (i => (depSize(i) + halo) `.` s)) ->:
            (allTiles `*.` (i => (depSize(i) `.` t)))) ->:
            ((n + halo) `.` s) ->: (n `.` t)
        }}})}
  }

  override def primitive: rise.core.Primitive = Primitive()()
  override def apply: ToBeTyped[rise.core.Primitive] = DSL.toBeTyped(Primitive()())
  override def unapply(arg: Expr): Boolean = arg match {
    case _: Primitive => true
    case _ => false
  }
}
