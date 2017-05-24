package idealised.OpenCL.IntermediatePrimitives

import idealised._
import idealised.DPIA.Compilation.SubstituteImplementations
import idealised.DPIA.Compilation.SubstituteImplementations._
import idealised.DPIA.DSL._
import idealised.DPIA.IntermediatePrimitives.AbstractMapI
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA._
import idealised.DPIA.DSL.λ
import idealised.OpenCL.ImperativePrimitives.ParForWorkGroup

final case class MapWorkGroupI(n: Nat,
                               dt1: DataType,
                               dt2: DataType,
                               f: Phrase[ExpType -> (AccType -> CommandType)],
                               in: Phrase[ExpType],
                               out: Phrase[AccType])
  extends AbstractMapI(n, dt1, dt2, f, in, out) {

  override def makeMapI = MapWorkGroupI

  override def substituteImpl(env: Environment): Phrase[CommandType] = {

    ParForWorkGroup(n, dt2, out, λ(exp"[idx($n)]")(i => λ(acc"[$dt2]")(a => {

      //      val access = (out `@` 0) `@` 0 // TODO: this is totally not generic ...
      //      TypeChecker(access)
      //      val identifier = ToOpenCL.acc(access, new ToOpenCL(?, ?))
      //      val addressSpace = env.addressspace(identifier.name)
      val addressSpace = OpenCL.GlobalMemory

      SubstituteImplementations(f(in `@` i)(a),
        env.copy(env.addressSpace.updated(a.name, addressSpace)))
    })))

  }

}
