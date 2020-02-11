package shine.OpenCL.IntermediatePrimitives

import shine.DPIA.DSL.{λ, _}
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, read}
import shine.DPIA.Types.DataType._
import shine.DPIA._
import shine.OpenCL.ImperativePrimitives.ParForWorkGroup

final case class MapWorkGroupI(dim: Int) {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: AccType ->: CommType],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    comment("mapWorkgroup")`;`
    ParForWorkGroup(dim)(n, dt2, out, λ(expT(idx(n), read))(i => λ(accT(dt2))(a => {

      //      val access = (out `@` 0) `@` 0 // TODO: this is totally not generic ...
      //      TypeChecker(access)
      //      val identifier = ToOpenCL.acc(access, new ToOpenCL(?, ?))
      //      val addressSpace = env.addressspace(identifier.name)
//      val addressSpace = AddressSpace.Global // FIXME: address space of 'a'

      f(in `@` i)(a)
    })))
  }
}
