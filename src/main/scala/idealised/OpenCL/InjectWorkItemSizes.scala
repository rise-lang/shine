package idealised.OpenCL

import idealised.DPIA.Phrases.{Phrase, VisitAndRebuild}
import idealised.DPIA.Types.{CommType, PhraseType}
import idealised.OpenCL.ImperativePrimitives.{OpenCLParFor, OpenCLParForNat, ParForGlobal, ParForLocal, ParForNatGlobal, ParForNatLocal, ParForNatWorkGroup, ParForWorkGroup}
import lift.arithmetic.{ContinuousRange, RangeAdd}

object InjectWorkItemSizes {
  def apply(localSize: Option[LocalSize], globalSize: Option[GlobalSize])
           (p: Phrase[CommType]): Phrase[CommType] = {

    if (localSize.isEmpty && globalSize.isEmpty) return p

    VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = p match {
        case f@OpenCLParFor(n, dt, out, body, _, step, _) => f match {
          case ParForGlobal(dim) => {
            val gSize = if (globalSize.isEmpty) step else globalSize.get.size (dim)
            val init = get_global_id(dim, ContinuousRange(0, gSize))
            Continue (ParForGlobal(dim)(n, dt, out, body, init, gSize), this)
          }
          case ParForLocal(dim) => {
            val lSize = if (localSize.isEmpty) step else localSize.get.size(dim)
            val init = get_local_id(dim, ContinuousRange(0, lSize))
            Continue (ParForLocal(dim)(n, dt, out, body, init, lSize), this)
          }
          case ParForWorkGroup(dim) => {
            val numGroups = if (localSize.isEmpty || globalSize.isEmpty) step
                            else globalSize.get.size(dim) /^ localSize.get.size(dim)
            val init = get_group_id(dim, ContinuousRange(0, numGroups))
            Continue (ParForWorkGroup(dim)(n, dt, out, body, init, numGroups), this)
          }
        }
        case f@OpenCLParForNat(n, ft, out, body, _, step, _) => f match {
          case ParForNatGlobal(dim) => {
            val gSize = if (globalSize.isEmpty) step else globalSize.get.size (dim)
            val init = get_global_id(dim, ContinuousRange(0, gSize))
            Continue (ParForNatGlobal(dim)(n, ft, out, body, init, gSize), this)
          }
          case ParForNatLocal(dim) => {
            val lSize = if (localSize.isEmpty) step else localSize.get.size(dim)
            val init = get_local_id(dim, ContinuousRange(0, lSize))
            Continue (ParForNatLocal(dim)(n, ft, out, body, init, lSize), this)
          }
          case ParForNatWorkGroup(dim) => {
            val numGroups = if (localSize.isEmpty || globalSize.isEmpty) step
            else globalSize.get.size(dim) /^ localSize.get.size(dim)
            val init = get_group_id(dim, ContinuousRange(0, numGroups))
            Continue (ParForNatWorkGroup(dim)(n, ft, out, body, init, numGroups), this)
          }
        }
        case _ => Continue(p, this)
      }
    })

  }
}
