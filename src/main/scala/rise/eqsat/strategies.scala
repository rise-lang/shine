package rise.eqsat

import scala.collection.mutable

object strategies {
  // returns whether the set has been modified
  type Strategy = ExprSet => Boolean
  // ExprSet => Option[ExprSet]

  def id: Strategy = _ => false

  def dotPrintTmp(prefix: String): Strategy = { es =>
    util.dotPrintTmp(prefix, es)
    id(es)
  }

  def normalize(s: Strategy): Strategy = es =>
    if (s(es)) { normalize(s)(es); true } else { false }

  def repeat(n: Int, s: Strategy): Strategy = es =>
    if (n > 0 && s(es)) { repeat(n - 1, s)(es); true } else { false }

  def any(ss: Strategy*): Strategy = es =>
    ss.exists(_(es))

  def seq(ss: Strategy*): Strategy = seqWithNorm(id, ss: _*)

  def seqWithNorm(norm: Strategy, ss: Strategy*): Strategy = { es =>
    var modified = false
    ss.foreach { s => if (s(es) | norm(es)) modified = true }
    modified
  }

  def topDown(r: rules.Rule, destructive: Boolean = false): Strategy = { rootes =>
    val visitedES = mutable.Set.empty[ExprSet]
    val visitedE = mutable.Set.empty[Expr]
    val results = mutable.ArrayBuffer.empty[(ExprSet, rules.Result)]
    def exprSetRec(es: ExprSet): Unit = {
      if (visitedES.contains(es)) { return }
      visitedES += es
      es.alternatives.foreach { e =>
        results += es -> r(e, es.t)
      }
      es.alternatives.foreach(exprRec)
    }
    def exprRec(e: Expr): Unit = {
      if (visitedE(e)) { return }
      visitedE += e
      e match {
        case _: Identifier => ()
        case Lambda(_, _, e) => exprSetRec(e)
        case App(f, e) => exprSetRec(f); exprSetRec(e)
        case DepLambda(_, e) => exprSetRec(e)
        case DepApp(f, _) => exprSetRec(f)
        case Literal(_) => ()
        case Primitive(_) => ()
      }
    }
    exprSetRec(rootes)
    var modified = false
    results.foreach { case (es, r) =>
      r.foreach { m =>
        if (updateSet(es, destructive)(m)) {
          modified = true
        }
      }
    }
    if (modified) { rootes.removeDuplicates() }
    if (modified && destructive) rootes.removeEmptySets()
    modified
  }

  def bottomUp(r: rules.Rule, destructive: Boolean = false): Strategy = { es =>
    val visited = mutable.Set.empty[ExprSet]
    var modified = false
    def exprSetRec(es: ExprSet): Unit = {
      if (!visited.contains(es)) {
        visited += es
        es.alternatives.foreach(exprRec)
        /*
        r(es).foreach { m =>
          if (updateSet(es, destructive)(m)) modified = true
        }

         */
      }
    }
    def exprRec: Expr => Unit = {
      case _: Identifier => ()
      case Lambda(_, _, e) => exprSetRec(e)
      case App(f, e) => exprSetRec(f); exprSetRec(e)
      case DepLambda(_, e) => exprSetRec(e)
      case DepApp(f, _) => exprSetRec(f)
      case Literal(_) => ()
      case Primitive(_) => ()
    }
    exprSetRec(es)
    if (modified && destructive) es.removeEmptySets()
    modified
  }

  // returns whether something was added to the set
  // destructive updates can create dead-ends (empty program sets)
  private def updateSet(es: ExprSet, destructive: Boolean): ((rules.Match, ExprSet)) => Boolean = {
    case (trace, r) =>
      if (destructive) {
        es.remove(trace)
      }
      es.add(r)
  }
}
