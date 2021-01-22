package shine.DPIA.primitives.functional

import arithexpr.arithmetic.BoolExpr.ArithPredicate.Operator
import arithexpr.arithmetic.BoolExpr.arithPredicate
import arithexpr.arithmetic.IfThenElse
import rise.core.types.Nat
import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

final case class DepTile(n: Nat, tileSize: Nat, haloSize: Nat,
                         dt1: DataType, dt2: DataType,
                         processTiles: Phrase[ExpType ->: ExpType],
                         array: Phrase[ExpType])
  extends ExpPrimitive
{
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

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    DepTile(f.nat(n), f.nat(tileSize), f.nat(haloSize), f.data(dt1), f.data(dt2),
      VisitAndRebuild(processTiles, f), VisitAndRebuild(array, f))
  }

  override def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = {
    ???
  }

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import shine.DPIA.Compilation.TranslationToImperative._

    // "DepSlide"
    // DepJoinAcc
    con(array)(fun(array.t)(x =>
      ???
    ))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???

  override def prettyPrint: String =
    s"(depSlide ${PrettyPhrasePrinter(processTiles)} ${PrettyPhrasePrinter(array)}"

  override def xmlPrinter: xml.Elem = <depTile></depTile>
}
