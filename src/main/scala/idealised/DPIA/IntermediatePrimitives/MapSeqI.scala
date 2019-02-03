package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.{TranslationContext, SubstituteImplementations}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

final case class MapSeqI(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         f: Phrase[ExpType -> (AccType -> CommandType)],
                         in: Phrase[ExpType],
                         out: Phrase[AccType])
  extends AbstractMapI(n, dt1, dt2, f, in, out) {

  override def makeMapI: (Nat, DataType, DataType, Phrase[ExpType -> (AccType -> CommandType)], Phrase[ExpType], Phrase[AccType]) => MapSeqI = MapSeqI

  override def substituteImpl(env: SubstituteImplementations.Environment)
                             (implicit context: TranslationContext): Phrase[CommandType] = {
    `for`(n, i =>
      SubstituteImplementations(f(in `@` i)(out `@` i), env)
    )
  }

}
