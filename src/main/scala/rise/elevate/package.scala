package rise

import _root_.elevate.core.Strategy
import _root_.elevate.core.strategies.Traversable
import _root_.rise.core.types.Type
import rise.core.exprs.Expr
import rise.core.exprs.primitives._
import rise.elevate.strategies.normalForm.DFNF

package object elevate {
  type Rise = Expr

  // type-extractor
  object ::: {
    def unapply(e: Expr): Option[(Expr, Type)] = Some((e, e.t))
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
