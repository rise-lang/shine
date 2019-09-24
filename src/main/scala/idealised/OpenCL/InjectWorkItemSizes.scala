package idealised.OpenCL

import idealised.DPIA.ImperativePrimitives.Assign
import idealised.DPIA.Nat
import idealised.DPIA.Phrases.{Phrase, VisitAndRebuild}
import idealised.DPIA.Types.{CommType, DataType, PhraseType}
import idealised.OpenCL.ImperativePrimitives.{IdxDistribute, IdxDistributeAcc, OpenCLNew, OpenCLParFor, OpenCLParForNat, ParForGlobal, ParForLocal, ParForNatGlobal, ParForNatLocal, ParForNatWorkGroup, ParForWorkGroup}
import lift.arithmetic.{ArithExpr, ContinuousRange, RangeAdd}

object InjectWorkItemSizes {
  def apply(localSize: Option[LocalSize], globalSize: Option[GlobalSize])
           (p: Phrase[CommType]): Phrase[CommType] = {

    if (localSize.isEmpty && globalSize.isEmpty) return p

    val lSizes: NDRange = if (localSize.isEmpty) (get_local_size(0), get_local_size(1), get_local_size(2))
                          else (localSize.get.size(0), localSize.get.size(1), localSize.get.size(2))

    val gSizes: NDRange = if (globalSize.isEmpty) (get_global_size(0), get_global_size(1), get_global_size(3))
                          else (globalSize.get.size(0), globalSize.get.size(1), globalSize.get.size(2))

    val numGroups: NDRange = if (localSize.isEmpty || globalSize.isEmpty)
                              (get_num_groups(0), get_num_groups(1), get_num_groups(2))
                             else (globalSize.get.size(0) /^ localSize.get.size(0),
                                   globalSize.get.size(1) /^ localSize.get.size(1),
                                   globalSize.get.size(2) /^ localSize.get.size(2))

    VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = p match {
        case f@OpenCLParFor(n, dt, out, body, _, step, _) => f match {
          case ParForGlobal(dim) => {
            val init = get_global_id(dim, ContinuousRange(0, gSizes(dim)))
            Continue (ParForGlobal(dim)(n, dt, out, body, init, gSizes(dim)), this)
          }
          case ParForLocal(dim) => {
            val init = get_local_id(dim, ContinuousRange(0, lSizes(dim)))
            Continue (ParForLocal(dim)(n, dt, out, body, init, lSizes(dim)), this)
          }
          case ParForWorkGroup(dim) => {
            val init = get_group_id(dim, ContinuousRange(0, numGroups(dim)))
            Continue (ParForWorkGroup(dim)(n, dt, out, body, init, numGroups(dim)), this)
          }
        }
        case f@OpenCLParForNat(n, ft, out, body, _, step, _) => f match {
          case ParForNatGlobal(dim) => {
            val init = get_global_id(dim, ContinuousRange(0, gSizes(dim)))
            Continue (ParForNatGlobal(dim)(n, ft, out, body, init, gSizes(dim)), this)
          }
          case ParForNatLocal(dim) => {
            val init = get_local_id(dim, ContinuousRange(0, lSizes(dim)))
            Continue (ParForNatLocal(dim)(n, ft, out, body, init, lSizes(dim)), this)
          }
          case ParForNatWorkGroup(dim) => {
            val init = get_group_id(dim, ContinuousRange(0, numGroups(dim)))
            Continue (ParForNatWorkGroup(dim)(n, ft, out, body, init, numGroups(dim)), this)
          }
        }
        case _ => Continue(p, this)
      }

      override def data[T <: DataType](dt: T): T =
        DataType.substitute(numGroups(2), get_num_groups(2),
          DataType.substitute(numGroups(1), get_num_groups(1),
            DataType.substitute(numGroups(0), get_num_groups(0),
              DataType.substitute(lSizes(2), get_local_size(2),
                DataType.substitute(lSizes(1), get_local_size(1),
                  DataType.substitute(lSizes(0), get_local_size(0),
                    DataType.substitute(gSizes(2), get_global_size(2),
                      DataType.substitute(gSizes(1), get_global_size(1),
                        DataType.substitute(gSizes(0), get_global_size(0), dt)))))))))

      override def nat[N <: Nat](n: N): N = {
        val substMap: Map[Nat, Nat] = Map(
          get_num_groups(2) -> numGroups(2),
          get_num_groups(1) -> numGroups(1),
          get_num_groups(0) -> numGroups(0),
          get_local_size(2) -> lSizes(2),
          get_local_size(1) -> lSizes(1),
          get_local_size(0) -> lSizes(0),
          get_global_size(2) -> gSizes(2),
          get_global_size(1) -> gSizes(1),
          get_global_size(0) -> gSizes(0)
        )

        val subst = ArithExpr.substitute(n, substMap).asInstanceOf[N]
        subst
      }
    })
  }
}
