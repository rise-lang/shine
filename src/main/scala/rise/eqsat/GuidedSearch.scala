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

    val goalSize = {
      val g = EGraph.emptyWithAnalysis(NoAnalysis)
      val id = g.addExpr(normGoal)
      val s = Analyser.init(g, AstSize)
      s.analysisOf(id)
    }
    println(s"goal size: ${goalSize}")

    @tailrec
    def rec(s: Int, egraph: DefaultAnalysis.EGraph, rootId: EClassId): Unit = {
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
        }.run(egraph, filter, rules, Seq(rootId))
        runner.printReport()
        if (!runner.stopReasons.contains(Done)) {
          throw CouldNotReachSnapshot(s, snapshot)
        }

        val g = EGraph.emptyWithAnalysis(analysis)
        g.hashConses = egraph.hashConses
        // TODO: should other known unions be restored?
        val r = matches.map { case (_, e) =>
          g.addExpr(e)
        }.reduce[EClassId] { case (a, b) => g.union(a, b)._1 }
        g.rebuild(Seq(rootId))
        rec(s + 1, g, r)
      } else {
        println(s"${egraph.nodeCount()} nodes, ${egraph.classCount()} classes")
      }
    }

    val g = EGraph.emptyWithAnalysis(analysis)
    rec(0, g, g.addExpr(normStart))
  }
}
