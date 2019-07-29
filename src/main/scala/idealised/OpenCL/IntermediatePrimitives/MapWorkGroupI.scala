package idealised.OpenCL.IntermediatePrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.DSL.{λ, _}
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommType, DataType, ExpType, read}
import idealised.DPIA._
import idealised.OpenCL.ImperativePrimitives.ParForWorkGroup
import idealised._

final case class MapWorkGroupI(dim: Int) {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: AccType ->: CommType],
            in: Phrase[ExpType],
            out: Phrase[AccType])
           (implicit context: TranslationContext): Phrase[CommType] =
  {
    ParForWorkGroup(dim)(n, dt2, out, λ(exp"[idx($n), $read]")(i => λ(acc"[$dt2]")(a => {

      //      val access = (out `@` 0) `@` 0 // TODO: this is totally not generic ...
      //      TypeChecker(access)
      //      val identifier = ToOpenCL.acc(access, new ToOpenCL(?, ?))
      //      val addressSpace = env.addressspace(identifier.name)
      val addressSpace = OpenCL.GlobalMemory // FIXME: address space of 'a'

      f(in `@` i)(a)
    })))
  }
}
