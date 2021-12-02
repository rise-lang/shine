package rise.eqsat

import scala.language.implicitConversions

case object CouldNotProveEquiv extends Exception

object ProveEquiv {
  def init(): ProveEquiv = new ProveEquiv(
    filter = NoPredicate(),
    transformRunner = r => r,
    endRules = Seq(),
    bidirectionalSearch = false,
  )

  case class OneOrMore[T](seq: Seq[T])

  object syntax {
    implicit def one[T](t: T): OneOrMore[T] = OneOrMore(Seq(t))
    implicit def more[T](s: Seq[T]): OneOrMore[T] = OneOrMore(s)
  }
}

class ProveEquiv(
  var filter: Predicate,
  var transformRunner: Runner => Runner,
  var endRules: Seq[Rewrite],
  var bidirectionalSearch: Boolean,
) {
  import ProveEquiv._

  def withFilter(filter: Predicate): ProveEquiv = {
    this.filter = filter
    this
  }

  def withRunnerTransform(f: Runner => Runner): ProveEquiv = {
    transformRunner = f
    this
  }

  def bidirectional(): ProveEquiv = {
    bidirectionalSearch = true
    this
  }

  def withEndRules(rs: Seq[Rewrite]): ProveEquiv = {
    endRules = rs
    this
  }

  def runBENF(starts: OneOrMore[rise.core.Expr],
              goals: OneOrMore[rise.core.Expr],
              rules: Seq[Rewrite],
              normRules: Seq[RewriteDirected] = BENF.directedRules): Unit = {
    val normStarts = starts.seq.map(s => BENF.normalize(Expr.fromNamed(s)))
    val normGoals = goals.seq.map(g => BENF.normalize(Expr.fromNamed(g)))
    for ((start, i) <- normStarts.zipWithIndex) {
      println(s"normalized start n°$i: ${Expr.toNamed(start)}")
    }
    for ((goal, i) <- normGoals.zipWithIndex) {
      println(s"normalized goal n°$i: ${Expr.toNamed(goal)}")
    }
    run(OneOrMore(normStarts), OneOrMore(normGoals), rules, normRules)
  }

  def runCNF(starts: OneOrMore[rise.core.Expr],
             goals: OneOrMore[rise.core.Expr],
             rules: Seq[Rewrite],
             normRules: Seq[RewriteDirected] = CNF.directedRules): Unit = {
    val normStarts = starts.seq.map(s => CNF(Expr.fromNamed(s)))
    val normGoals = goals.seq.map(g => CNF(Expr.fromNamed(g)))
    for ((start, i) <- normStarts.zipWithIndex) {
      println(s"normalized start n°$i: ${Expr.toNamed(start)}")
    }
    for ((goal, i) <- normGoals.zipWithIndex) {
      println(s"normalized goal n°$i: ${Expr.toNamed(goal)}")
    }
    run(OneOrMore(normStarts), OneOrMore(normGoals), rules, normRules)
  }

  def run(starts: OneOrMore[Expr],
          goals: OneOrMore[Expr],
          rules: Seq[Rewrite],
          normRules: Seq[RewriteDirected]): Unit = {
    val egraph = EGraph.empty()
    egraph.requireAnalyses(filter.requiredAnalyses())
    val startId = starts.seq.tail.foldLeft(egraph.addExpr(starts.seq.head)) { case (id, e) =>
      egraph.union(id, egraph.addExpr(e))._1
    }

    if (bidirectionalSearch) {
      runBidirectional(egraph, startId, goals.seq, rules, normRules)
    } else {
      runUnidirectional(egraph, startId, goals.seq, rules, normRules)
    }
  }

  private def runUnidirectional(egraph: EGraph,
                                startId: EClassId,
                                goals: Seq[Expr],
                                rules: Seq[Rewrite],
                                normRules: Seq[RewriteDirected]): Unit = {
    var remainingGoals = goals

    def goalReached(g: Expr): Boolean =
      egraph.lookupExpr(g).contains(egraph.find(startId))
    val runner = transformRunner(Runner.init()).doneWhen { r =>
      util.printTime("goal check", {
        remainingGoals = remainingGoals.filterNot(goalReached)
        remainingGoals.isEmpty
      })
    }.run(egraph, filter, rules, normRules, Seq(startId))
    afterRun(runner, egraph, startId, goals, i => goalReached(goals(i)))
  }

  private def runBidirectional(egraph: EGraph,
                               startId: EClassId,
                               goals: Seq[Expr],
                               rules: Seq[Rewrite],
                               normRules: Seq[RewriteDirected]): Unit = {
    val goalIds = goals.map(egraph.addExpr)
    val runner = transformRunner(Runner.init()).doneWhen { _ =>
      goalIds.forall(g => egraph.findMut(startId) == egraph.findMut(g))
    }.run(egraph, filter, rules, normRules, startId +: goalIds)
    afterRun(runner, egraph, startId, goals, {
      i => egraph.findMut(startId) == egraph.findMut(goalIds(i))
    })

    goals.foreach { g =>
      assert(egraph.lookupExpr(g).contains(egraph.find(startId)))
    }
  }

  private def afterRun(runner: Runner,
                       egraph: EGraph,
                       startId: EClassId,
                       goals: Seq[Expr],
                       goalReached: Int => Boolean): Unit = {
    runner.printReport()

    // egraph.dot().toSVG("/tmp/e-graph.svg")
    if (!runner.stopReasons.contains(Done)) {
      runner.iterations.foreach(println)
      val (found, notFound) = goals.indices.partition(goalReached)

      val idsToFind = notFound.map(i => egraph.addExpr(goals(i)))
      val endRunner = Runner.init().doneWhen { _ =>
        idsToFind.forall(id => egraph.findMut(startId) == egraph.findMut(id))
      }.run(egraph, NoPredicate(), endRules, Seq(), Seq(startId))
      if (endRunner.stopReasons.contains(Done)) {
        return
      }
      
      println(s"found: ${found.mkString(", ")}")
      val (endFound, neverFound) = notFound.zip(idsToFind).partition { case (_, id) =>
        egraph.findMut(startId) == egraph.findMut(id)
      }
      println(s"found at the end: ${endFound.map(_._1).mkString(", ")}")
      println(s"never found: ${neverFound.map(_._1).mkString(", ")}")
      throw CouldNotProveEquiv
    }
  }
}
