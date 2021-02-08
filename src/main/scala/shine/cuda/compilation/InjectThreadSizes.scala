package shine.cuda.compilation

import arithexpr.arithmetic.ArithExpr
import shine.DPIA.Nat
import shine.DPIA.Phrases.{Phrase, VisitAndRebuild}
import shine.DPIA.Types.{CommType, DataType}
import shine.OpenCL.{GlobalSize, LocalSize, NDRange, get_local_size, get_num_groups}
import shine.cuda.{blockDim, gridDim}

object InjectThreadSizes {
  def inject(localSize: Option[LocalSize],
             globalSize: Option[GlobalSize]): Phrase[CommType] => Phrase[CommType] = p => {
    if (localSize.isEmpty && globalSize.isEmpty) {
      p
    } else {
      val lSizes: NDRange = if (localSize.isEmpty) (get_local_size(0), get_local_size(1), get_local_size(2))
      else (localSize.get.size(0), localSize.get.size(1), localSize.get.size(2))

      val numGroups: NDRange = if (localSize.isEmpty || globalSize.isEmpty)
        (get_num_groups(0), get_num_groups(1), get_num_groups(2))
      else (globalSize.get.size(0) /^ localSize.get.size(0),
        globalSize.get.size(1) /^ localSize.get.size(1),
        globalSize.get.size(2) /^ localSize.get.size(2))

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
