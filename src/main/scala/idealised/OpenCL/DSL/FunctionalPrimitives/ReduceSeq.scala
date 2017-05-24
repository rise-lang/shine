package idealised.OpenCL.DSL.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped._
import idealised.DSL.untyped.FunctionalPrimitives.AbstractReduce

final case class ReduceSeq(f: Expr[ExpType -> (ExpType -> ExpType)],
                           init: DataExpr,
                           array: DataExpr) extends AbstractReduce(f, init, array) {

  override def makeReduce: (Expr[->[ExpType, ->[ExpType, ExpType]]], DataExpr, DataExpr) =>
    AbstractReduce = ReduceSeq

  override def makePhraseReduce: (Nat, DataType, DataType, Phrase[->[ExpType, ->[ExpType, ExpType]]], Phrase[ExpType], Phrase[ExpType]) =>
    idealised.FunctionalPrimitives.AbstractReduce = idealised.OpenCL.FunctionalPrimitives.ReduceSeq
}
