package shine.DPIA.primitives.functional

import arithexpr.arithmetic.BoolExpr.ArithPredicate.Operator
import arithexpr.arithmetic.BoolExpr.arithPredicate
import arithexpr.arithmetic.{IfThenElse, SimplifiedExpr}
import rise.core.types.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._

final case class DepTile(n: Nat, tileSize: Nat, haloSize: Nat,
                         dt1: DataType, dt2: DataType,
                         processTiles: Phrase[ExpType ->: ExpType],
                         array: Phrase[ExpType]
                        ) extends ExpPrimitive {
  val allTiles: Nat = (n + tileSize - 1) / tileSize
  val fullTiles: Nat = n / tileSize
  val remainder: Nat = n % tileSize

  private def depSize(i: Nat): Nat =
    IfThenElse(arithPredicate(i, fullTiles, Operator.<), tileSize, remainder)

  processTiles :: (
    expT(allTiles `.d` (i => (depSize(i) + haloSize) `.` dt1), read) ->:
    expT(allTiles `.d` (i => (depSize(i) `.` dt2)), write))
  array :: expT((n + haloSize)`.`dt1, read)
  override val t: ExpType = expT(n`.`dt2, write)

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] =
    DepTile(v.nat(n), v.nat(tileSize), v.nat(haloSize), v.data(dt1), v.data(dt2),
      VisitAndRebuild(processTiles, v), VisitAndRebuild(array, v))
}
