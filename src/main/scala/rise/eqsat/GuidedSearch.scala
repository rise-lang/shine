package rise.eqsat

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

    def rec(s: Int, egraph: DefaultAnalysis.EGraph, rootId: EClassId): Unit = {
      if (s < snapshots.length) {
        val snapshot = snapshots(s)
        var matches = Vec.empty[ExtendedPatternMatch]
        val runner = transformRunner(Runner.init()).doneWhen { r =>
          util.printTime("goal check", {
            matches = snapshot.searchEClass(egraph, rootId)
            matches.nonEmpty
          })
        }.run(egraph, filter, rules, Seq(rootId))
        runner.printReport()
        if (!runner.stopReasons.contains(Done)) {
          throw CouldNotReachSnapshot(s, snapshot)
        }

        val (g, r) = ExtendedPattern.matchesToGraph(matches, egraph, analysis)
        rec(s + 1, g, r)
      }
    }

    val g = EGraph.emptyWithAnalysis(analysis)
    rec(0, g, g.addExpr(normStart))
  }
}
