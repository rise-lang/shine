package shine.OpenCL.Compilation.Passes

import arithexpr.arithmetic.ArithExpr
import rise.core.types.DataType
import rise.core.substitute.{natInType => substituteNatInType}
import shine.DPIA.Nat
import shine.DPIA.Phrases.{Phrase, VisitAndRebuild}
import shine.DPIA.Types.CommType
import shine.OpenCL._

object InjectWorkItemSizes {
  def inject(wgConfig: Option[(LocalSize, GlobalSize)]
            ): Phrase[CommType] => Phrase[CommType] = p => {
    wgConfig match {
      case None => p
      case Some((localSize, globalSize)) =>
        val lSizes = localSize.size
        val gSizes = globalSize.size
        val numGroups = NDRange(
          gSizes(0) /^ lSizes(0),
          gSizes(1) /^ lSizes(1),
          gSizes(2) /^ lSizes(2))

        VisitAndRebuild(p, new VisitAndRebuild.Visitor {
          override def data[T <: DataType](dt: T): T =
            substituteNatInType(numGroups(2), get_num_groups(2),
              substituteNatInType(numGroups(1), get_num_groups(1),
                substituteNatInType(numGroups(0), get_num_groups(0),
                  substituteNatInType(lSizes(2), get_local_size(2),
                    substituteNatInType(lSizes(1), get_local_size(1),
                      substituteNatInType(lSizes(0), get_local_size(0),
                        substituteNatInType(gSizes(2), get_global_size(2),
                          substituteNatInType(gSizes(1), get_global_size(1),
                            substituteNatInType(gSizes(0), get_global_size(0),
                              dt)))))))))

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
