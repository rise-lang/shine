package rise

import _root_.rise.core._
import _root_.rise.core.primitives._
import _root_.rise.core.types.ExprType
import rise.elevate.strategies.normalForm.DFNF
import _root_.elevate.core.Strategy
import _root_.elevate.core.strategies.Traversable

package object elevate {
  type Rise = Expr

  // type-extractor
  object ::: {
    def unapply(e: Expr): Option[(Expr, ExprType)] = Some((e, e.t))
  }

  object ReduceX {
    def unapply(e: Expr): Boolean = e match {
      case reduce() => true
      case reduceSeq() => true
      case reduceSeqUnroll() => true
      case _ => false
    }
  }


//   scalastyle:off
     implicit class NormalizedThen(f: Strategy[Rise])(implicit ev: Traversable[Rise]) {
       def `;;`(s: Strategy[Rise]): Strategy[Rise] = f `;` DFNF() `;` s
     }
//   scalastyle:on

}
