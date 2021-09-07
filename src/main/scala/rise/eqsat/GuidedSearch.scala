package rise.eqsat

import scala.annotation.tailrec

case class CouldNotReachSnapshot(i: Int, snapshot: ExtendedPattern) extends Exception

object GuidedSearch {
  def init(): GuidedSearch = new GuidedSearch(
    filter = NoPredicate(),
    analysis = DefaultAnalysis,
    transformRunner = r => r,
  )
}

class GuidedSearch(
  var filter: DefaultAnalysis.Predicate,
  var analysis: DefaultAnalysisCustomisable,
  var transformRunner: Runner => Runner
) {
  def withFilter(filter: DefaultAnalysis.Predicate): GuidedSearch = {
    this.filter = filter
    this
  }

  def withAnalysis(analysis: DefaultAnalysisCustomisable): GuidedSearch = {
    this.analysis = analysis
    this
  }

  def withRunnerTransform(f: Runner => Runner): GuidedSearch = {
    transformRunner = f
    this
  }

  def runBENF(start: rise.core.Expr,
              goal: rise.core.Expr,
              rules: Seq[DefaultAnalysis.Rewrite],
              snapshots: Seq[ExtendedPattern]): Unit = {
    val normStart = BENF(Expr.fromNamed(start))
    val normGoal = BENF(Expr.fromNamed(goal))
    println(s"normalized start: ${Expr.toNamed(normStart)}")
    println(s"normalized goal: ${Expr.toNamed(normGoal)}")

    @tailrec
    def rec(s: Int, egraph: DefaultAnalysis.EGraph, rootId: EClassId): Unit = {
      egraph.rebuild(Seq(rootId))
      println("----")
      val pcount = Analyser.init(egraph, CountProgramsUpToSize(120))
      pcount.analysisOf(rootId).foreach { case (size, count) =>
        println(s"programs of size ${size}: ${count}")
      }
      println("----")
      // egraph.dot().toSVG(s"/tmp/e-graph-$s.svg")

      if (s < snapshots.length) {
        val snapshot = snapshots(s)
        var matches = Vec.empty[ExtendedPatternMatch]
        val runner = transformRunner(Runner.init()).doneWhen { r =>
          util.printTime("goal check", {
            // if (r.iterationCount() % 3 == 0) {
            matches = snapshot.searchEClass(egraph, rootId)
            // }
            matches.nonEmpty
          })
        }.run(egraph, filter, rules, Seq(rootId))
        runner.printReport()
        if (!runner.stopReasons.contains(Done)) {
          throw CouldNotReachSnapshot(s, snapshot)
        }

        val (g, r) = util.printTime("matches to graph",
          ExtendedPattern.matchesToGraph(matches, egraph, analysis))
        rec(s + 1, g, r)
      } else {
        println(s"${egraph.nodeCount()} nodes, ${egraph.classCount()} classes")
        egraph.rebuild(Seq(rootId))
        println(s"${egraph.nodeCount()} nodes, ${egraph.classCount()} classes")
      }
    }

    val g = EGraph.emptyWithAnalysis(analysis)
    rec(0, g, g.addExpr(normStart))
  }
}
