package rise.eqsat

import scala.annotation.tailrec

case class CouldNotReachSnapshot(i: Int, snapshot: ExtendedPattern) extends Exception

object GuidedSearch {
  def init(): GuidedSearch = new GuidedSearch(
    filter = NoPredicate(),
    transformRunner = r => r,
  )
}

class GuidedSearch(
  var filter: Predicate,
  var transformRunner: Runner => Runner
) {
  def withFilter(filter: Predicate): GuidedSearch = {
    this.filter = filter
    this
  }

  def withRunnerTransform(f: Runner => Runner): GuidedSearch = {
    transformRunner = f
    this
  }

  def runBENF(start: rise.core.Expr,
              goal: rise.core.Expr,
              rules: Seq[Rewrite],
              snapshots: Seq[ExtendedPattern]): Unit = {
    val normStart = BENF(Expr.fromNamed(start))
    val normGoal = BENF(Expr.fromNamed(goal))
    println(s"normalized start: ${Expr.toNamed(normStart)}")
    println(s"normalized goal: ${Expr.toNamed(normGoal)}")

    val goalSize = {
      val g = EGraph.empty()
      val id = g.addExpr(normGoal)
      val s = Analyser.init(g, AstSize)
      s.analysisOf(id)
    }
    println(s"goal size: ${goalSize}")

    @tailrec
    def rec(s: Int, egraph: EGraph, rootId: EClassId): Unit = {
      egraph.rebuild(Seq(rootId))
      println("----")
      val pcount = Analyser.init(egraph, CountProgramsUpToSize(goalSize + 10))
      pcount.analysisOf(rootId).foreach { case (size, count) =>
        println(s"programs of size ${size}: ${count}")
      }
      BeamExtract.print(6, AstSize, egraph, rootId)
      println("----")

      if (s < snapshots.length) {
        val snapshot = snapshots(s)
        var matches = Seq[(Int, ExprWithHashCons)]()
        // TODO: add beam analysis to e-graph for incremental update
        val runner = transformRunner(Runner.init()).doneWhen { r =>
          util.printTime("goal check", {
            matches = ExtendedPattern.beamSearch(snapshot, 6, AstSize, egraph, rootId)
            matches.nonEmpty
          })
        }.run(egraph, filter, rules, Seq(), Seq(rootId))
        runner.printReport()
        if (!runner.stopReasons.contains(Done)) {
          throw CouldNotReachSnapshot(s, snapshot)
        }

        val g = EGraph.empty()
        g.hashConses = egraph.hashConses
        // TODO: should other known unions be restored?
        val r = matches.map { case (_, e) =>
          g.addExpr(e)
        }.reduce[EClassId] { case (a, b) => g.union(a, b)._1 }
        rec(s + 1, g, r)
      } else {
        println(s"${egraph.nodeCount()} nodes, ${egraph.classCount()} classes")
      }
    }

    val g = EGraph.empty()
    rec(0, g, g.addExpr(normStart))
  }
}
