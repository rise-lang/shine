package shine.cuda.Compilation.Passes

import arithexpr.arithmetic.ArithExpr
import shine.DPIA.Nat
import shine.DPIA.Phrases.{Phrase, VisitAndRebuild}
import shine.DPIA.Types.{CommType, DataType}
import shine.OpenCL.{GlobalSize, LocalSize, NDRange, get_local_size, get_num_groups}
import shine.cuda.{blockDim, gridDim}

object InjectThreadSizes {
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
            DataType.substitute(numGroups(2), gridDim('z'),
              DataType.substitute(numGroups(1), gridDim('y'),
                DataType.substitute(numGroups(0), gridDim('x'),
                  DataType.substitute(lSizes(2), blockDim('z'),
                    DataType.substitute(lSizes(1), blockDim('y'),
                      DataType.substitute(lSizes(0), blockDim('x'), dt))))))

          override def nat[N <: Nat](n: N): N = {
            val substMap: Map[Nat, Nat] = Map(
              gridDim('z') -> numGroups(2),
              gridDim('y') -> numGroups(1),
              gridDim('x') -> numGroups(0),
              blockDim('z') -> lSizes(2),
              blockDim('y') -> lSizes(1),
              blockDim('x') -> lSizes(0),
            )

            val subst = ArithExpr.substitute(n, substMap).asInstanceOf[N]
            subst
          }
        })
    }
  }
}
