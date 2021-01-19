package shine.DPIA.primitives.functional

import arithexpr.arithmetic.BoolExpr.ArithPredicate.Operator
import arithexpr.arithmetic.BoolExpr.arithPredicate
import arithexpr.arithmetic.IfThenElse
import rise.core.types.Nat
import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class DepTile(n: Nat, tileSize: Nat, haloSize: Nat,
                         dt1: DataType, dt2: DataType,
                         processTiles: Phrase[ExpType ->: ExpType],
                         array: Phrase[ExpType]
                        ) extends ExpPrimitive with AccT {
  val allTiles = (n + tileSize - 1) / tileSize
  val fullTiles = n / tileSize
  val remainder = n % tileSize

  private def depSize(i: Nat): Nat =
    IfThenElse(arithPredicate(i, fullTiles, Operator.<), tileSize, remainder)

  processTiles :: (
    expT(allTiles `.d` (i => (depSize(i) + haloSize) `.` dt1), read) ->:
    expT(allTiles `.d` (i => (depSize(i) `.` dt2)), write))
  array :: expT((n + haloSize)`.`dt1, read)
  override val t: ExpType = expT(n`.`dt2, write)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    // "DepSlide"
    // DepJoinAcc
    con(array)(fun(array.t)(x => ???))
}
