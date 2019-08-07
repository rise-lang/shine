package FSmooth

import FSmooth.traversal._

object substitute {
  def apply(exprs: Seq[Expr], `for`: Seq[Expr], in: Expr): Expr = {
    object Visitor extends traversal.Visitor {
      override def apply(e: Expr): Result[Expr] = {
        val pos = `for`.indexOf(e)
        if (pos != -1) {
          Stop(exprs(pos))
        } else {
          Continue(e, this)
        }
      }
    }

    traversal.DepthFirstLocalResult(in, Visitor)
  }
}
