package rise.elevate.rules

import elevate.core.{Strategy, Success}
import elevate.macros.RuleMacro.rule
import rise.core.App
import rise.core.DSL._
import rise.core.primitives.{add, split, zip, let => _, _}
import rise.core.types.AddressSpace
import rise.elevate.Rise
import rise.openCL.primitives._

object workEfficientScan {
  @rule def expandBlockScan: Strategy[Rise] = {
    case e @ App(App(App(scan(), _), init), in) =>
      def sum = oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))

      val input = preserveType(in)
      val rewritten = let(input |> split(2) |> mapLocal(0)(sum) |> oclToMem(AddressSpace.Local))
        .be(next => {
          val upsweep = scan(add)(init)(next) |> oclToMem(AddressSpace.Local)
          zip(upsweep)(input |> split(2)) |>
            mapLocal(0)(fun(pair => oclScanSeqUnroll(AddressSpace.Private)(add)(pair._1)(pair._2)))|>
            join
        })
      Success(rewritten !: e.t)
  }

  @rule def lowerToSequential: Strategy[Rise] = {
    case e @ App(App(App(scan(), _), init), in) =>
      Success(oclScanSeqUnroll(AddressSpace.Private)(add)(init)(in) !: e.t)
  }
}