package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.RewriteToImperative
import idealised.DPIA._
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._

abstract class AbstractMapLoop(n: Nat,
                               dt1: DataType,
                               dt2: DataType,
                               f: Phrase[ExpType -> ExpType],
                               array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array)
{
  def makeMapI: (Nat, DataType, DataType,
    Phrase[ExpType -> (AccType -> CommandType)], Phrase[ExpType], Phrase[AccType]) => Phrase[CommandType]


  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._

    con(array)(λ(exp"[$n.$dt1]")(x =>
      makeMapI(n, dt1, dt2, λ(exp"[$dt1]")(x => λ(acc"[$dt2]")(o => acc(f(x))(o))), x, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    import RewriteToImperative._

    `new`(dt"[$n.$dt2]", idealised.OpenCL.GlobalMemory, λ(exp"[$n.$dt2]" x acc"[$n.$dt2]")(tmp =>
      acc(this)(tmp.wr) `;` C(tmp.rd) ))
  }
}
