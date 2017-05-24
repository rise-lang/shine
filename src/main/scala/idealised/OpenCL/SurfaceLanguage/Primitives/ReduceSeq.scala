package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.DPIA._
import idealised.DPIA.Types.{DataType, ExpType}
import idealised.DPIA.Phrases.Phrase
import idealised.OpenCL
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Primitives.AbstractReduce

final case class ReduceSeq(f: Expr[ExpType -> (ExpType -> ExpType)],
                           init: DataExpr,
                           array: DataExpr) extends AbstractReduce(f, init, array) {

  override def makeReduce: (Expr[->[ExpType, ->[ExpType, ExpType]]], DataExpr, DataExpr) =>
    AbstractReduce = ReduceSeq

  override def makePhraseReduce: (Nat, DataType, DataType, Phrase[->[ExpType, ->[ExpType, ExpType]]], Phrase[ExpType], Phrase[ExpType]) =>
    idealised.DPIA.FunctionalPrimitives.AbstractReduce = OpenCL.FunctionalPrimitives.ReduceSeq
}
