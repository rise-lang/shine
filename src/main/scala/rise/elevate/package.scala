package rise

import _root_.rise.core._
import _root_.rise.core.primitives._
import _root_.rise.core.types.Type
import _root_.rise.core.DSL.{TypeAnnotationHelper, infer}
import rise.elevate.strategies.normalForm.DFNF
import _root_.elevate.core._
import _root_.elevate.core.strategies.Traversable

package object elevate {
  type Rise = Expr

  // type-extractor
  object ::: {
    def unapply(e: Expr): Option[(Expr, Type)] = Some((e, e.t))
  }

  def rewrite : Rise => Rise => Rise = lhs => rhs =>
    infer(rhs :: lhs.t, infer.collectFreeEnv(lhs), infer.getFTVsRec(lhs).map(_.name))

  def rewriteS : Strategy[Strategy[Rise]] =
    f => Success(lhs => f(lhs).flatMapSuccess(rhs => Success(rewrite(lhs)(rhs))))

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
