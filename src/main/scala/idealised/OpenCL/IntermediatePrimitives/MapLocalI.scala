package idealised.OpenCL.IntermediatePrimitives

import idealised.DPIA.Compilation.{TranslationContext, SubstituteImplementations}
import idealised.DPIA.Compilation.SubstituteImplementations._
import idealised.DPIA.DSL.{λ, _}
import idealised.DPIA.IntermediatePrimitives.AbstractMapI
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA._
import idealised.OpenCL.ImperativePrimitives.ParForLocal

final case class MapLocalI(dim: Int)(n: Nat,
                                     dt1: DataType,
                                     dt2: DataType,
                                     f: Phrase[ExpType -> (AccType -> CommandType)],
                                     in: Phrase[ExpType],
                                     out: Phrase[AccType])
  extends AbstractMapI(n, dt1, dt2, f, in, out) {

  override def makeMapI = MapLocalI(dim)

  override def substituteImpl(env: Environment)
                             (implicit context: TranslationContext): Phrase[CommandType] = {
    ParForLocal(dim)(n, dt2, out, λ(exp"[idx($n)]")(i => λ(acc"[$dt2]")(a =>
      SubstituteImplementations(f(in `@` i)(a), env)
    )))
  }

}
