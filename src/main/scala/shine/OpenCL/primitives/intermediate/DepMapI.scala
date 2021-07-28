package shine.OpenCL.primitives.intermediate

import rise.core.types.NatToData
import shine.DPIA.DSL._
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.DSL._
import shine.OpenCL._

final case class DepMapI(level: ParallelismLevel, dim: Int) {
  def apply(n: Nat,
            ft1:NatToData,
            ft2:NatToData,
            f: Phrase[`(nat)->:`[ExpType ->: AccType ->: CommType]],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] = {
    level match {
      case Global =>
        parForNatGlobal(dim)(n, ft2, out, idx => a => f(idx)(in `@d` idx)(a))
      case Local =>
        parForNatLocal(dim)(n, ft2, out, idx => a => f(idx)(in `@d` idx)(a)) `;` barrier()
      case WorkGroup =>
        parForNatWorkGroup(dim)(n, ft2, out, idx => a => f(idx)(in `@d` idx)(a))
      case Sequential | Warp | Lane =>
        throw new Exception("This should not happen")
    }
  }
}
