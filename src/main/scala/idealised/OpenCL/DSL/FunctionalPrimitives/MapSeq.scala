package idealised.OpenCL.DSL.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped._
import idealised.DSL.untyped.FunctionalPrimitives.AbstractMap

final case class MapSeq(f: Expr[ExpType -> ExpType],
                           array: DataExpr) extends AbstractMap(f, array) {
  override def makeMap: (Expr[->[ExpType, ExpType]], DataExpr) => AbstractMap = MapSeq

  override def makePhraseMap: (Nat, DataType, DataType, Phrase[->[ExpType, ExpType]], Phrase[ExpType]) =>
    idealised.FunctionalPrimitives.AbstractMap = idealised.OpenCL.FunctionalPrimitives.MapSeq
}
