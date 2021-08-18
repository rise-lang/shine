package rise.elevate.rules

import elevate.core.{Strategy, Success}
import elevate.macros.RuleMacro.rule
import rise.core.DSL._
import rise.core.primitives.{add, split, transpose, zip, let => _, _}
import rise.core.types.{AddressSpace, AddressSpaceKind}
import rise.core.{App, DepApp, Lambda}
import rise.elevate.Rise
import rise.openCL.primitives._

object scan {
  @rule def removeTransposePair: Strategy[Rise] = {
    case e @ App(transpose(), App(transpose(), x)) =>
      Success(x !: e.t)
  }

  @rule def blockScanStepRec: Strategy[Rise] = {
    case e @ App(App(App(DepApp(AddressSpaceKind, oclScanSeq(), address: AddressSpace), _), init), in) =>
      def sum = oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))

      val input = preserveType(in)
      val rewritten = let(input |> split(2) |> mapLocal(0)(sum) |> oclToMem(AddressSpace.Local))
        .be(next => {
          val upsweep = oclScanSeq(address)(add)(init)(next) |> oclToMem(AddressSpace.Local)
          zip(upsweep)(input |> split(2)) |>
            mapLocal(0)(fun(pair => oclScanSeqUnroll(AddressSpace.Private)(add)(pair._1)(pair._2)))|>
            join
        })
      Success(rewritten !: e.t)
  }
}
