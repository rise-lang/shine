package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.DPIA._
import idealised.DPIA.Types.{DataType, ExpType}
import idealised.DPIA.Phrases.Phrase
import idealised.OpenCL
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Primitives.AbstractMap

final case class MapSeq(f: Expr[ExpType -> ExpType],
                           array: DataExpr) extends AbstractMap(f, array) {
  override def makeMap: (Expr[->[ExpType, ExpType]], DataExpr) => AbstractMap = MapSeq

  override def makePhraseMap: (Nat, DataType, DataType, Phrase[->[ExpType, ExpType]], Phrase[ExpType]) =>
    idealised.DPIA.FunctionalPrimitives.AbstractMap = OpenCL.FunctionalPrimitives.MapSeq
}
