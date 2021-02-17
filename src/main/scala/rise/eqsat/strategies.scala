package rise.eqsat

import scala.collection.mutable

object strategies {
  // returns whether the set has been modified
  type Strategy = ExprSet => Boolean

  def id: Strategy = _ => false

  def normalize(s: Strategy): Strategy = es =>
    if (s(es)) { normalize(s)(es); true } else { false }

  def any(ss: Strategy*): Strategy = es =>
    ss.exists(_(es))

  def seq(ss: Strategy*): Strategy = seqWithNorm(id, ss: _*)

  def seqWithNorm(norm: Strategy, ss: Strategy*): Strategy = { es =>
    var modified = false
    ss.foreach { s => if (s(es) | norm(es)) modified = true }
    modified
  }

  def applyTopDown(r: rules.Rule, destructive: Boolean = false): Strategy = { es =>
    val visited = mutable.Set.empty[ExprSet]
    val visited2 = mutable.Set.empty[Expr]
    var modified = false
    def exprSetRec(es: ExprSet): Unit = {
      if (!visited.contains(es)) {
        visited += es
        r(es).foreach { m =>
          if (updateSet(es, destructive)(m)) modified = true
        }
        es.alternatives.foreach(exprRec)
      }
    }
    def exprRec: Expr => Unit = { e =>
      if (!visited2.contains(e)) {
        visited2 += e
      } else {
        println(s"visited expr: ${System.identityHashCode(e)}")
      }
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
    exprSetRec(es)
    if (modified) es.removeEmptySets()
    modified
  }

  def applyBottomUp(r: rules.Rule, destructive: Boolean = false): Strategy = { es =>
    val visited = mutable.Set.empty[ExprSet]
    var modified = false
    def exprSetRec(es: ExprSet): Unit = {
      if (!visited.contains(es)) {
        visited += es
        es.alternatives.foreach(exprRec)
        r(es).foreach { m =>
          if (updateSet(es, destructive)(m)) modified = true
        }
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
    if (modified) es.removeEmptySets()
    modified
  }

  // returns whether something was added to the set
  // destructive updates can create dead-ends (empty program sets)
  private def updateSet(es: ExprSet, destructive: Boolean): rules.Matches => Boolean = {
    case (trace, r) =>
      if (destructive) {
        es.remove(trace)
      }
      es.add(r)
  }
}
