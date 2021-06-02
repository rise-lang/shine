package rise.elevate

import rise.core._
import rise.core.primitives._
import rise.core.types.Type
import rise.elevate.strategies.normalForm.DFNF
import elevate.core.Strategy
import elevate.core.strategies.Traversable

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
