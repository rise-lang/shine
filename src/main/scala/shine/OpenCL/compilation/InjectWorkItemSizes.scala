package shine.OpenCL.compilation

import arithexpr.arithmetic.ArithExpr
import shine.DPIA.Nat
import shine.DPIA.Phrases.{Phrase, VisitAndRebuild}
import shine.DPIA.Types.{CommType, DataType}
import shine.OpenCL._

object InjectWorkItemSizes {
  def inject(localSize: Option[LocalSize],
             globalSize: Option[GlobalSize]): Phrase[CommType] => Phrase[CommType] = p => {
      if (localSize.isEmpty && globalSize.isEmpty) {
        p
      } else {
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
}
