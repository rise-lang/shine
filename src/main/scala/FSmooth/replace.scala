package FSmooth

import FSmooth.traversal.{Continue, Result, Stop}

//noinspection TypeAnnotation,ApparentResultTypeRefinement
object replace {
  def apply(e: Expr): Object {
    def `with` (w: Expr): Object {
      def in (i: Expr): Expr
    }
  } = new {
    def `with`(w: Expr): Object {
      def in(i: Expr): Expr
    } = new {
      def in(i: Expr): Expr = {
        replace(Seq(e)).`with`(Seq(w)).in(i)
      }
    }
  }

  def apply(es: Seq[Expr]): Object {
    def `with` (ws: Seq[Expr]): Object {
      def in (i: Expr): Expr
    }
  } = new {
    def `with`(ws: Seq[Expr]): Object {
      def in(i: Expr): Expr
    } = new {
      def in(i: Expr): Expr = {
        object Visitor extends traversal.Visitor {
          override def apply(e: Expr): Result[Expr] = {
            val pos = es.indexOf(e)
            if (pos != -1) {
              Stop(ws(pos))
            } else {
              Continue(e, this)
            }
          }
        }

        traversal.DepthFirstLocalResult(i, Visitor)
      }
    }
  }
}
