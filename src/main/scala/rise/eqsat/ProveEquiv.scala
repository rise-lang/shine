package rise.eqsat

case object CouldNotProveEquiv extends Exception

object ProveEquiv {
  def init(): ProveEquiv = new ProveEquiv(
    filter = NoPredicate(),
    analysis = DefaultAnalysis,
    arrayLimit = 10,
    transformRunner = r => r
  )
}

class ProveEquiv(
  var filter: DefaultAnalysis.Predicate,
  var analysis: DefaultAnalysisCustomisable,
  var arrayLimit: Int,
  var transformRunner: Runner => Runner,
) {
  def withFilter(filter: DefaultAnalysis.Predicate): ProveEquiv = {
    this.filter = filter
    this
  }

  def withAnalysis(analysis: DefaultAnalysisCustomisable): ProveEquiv = {
    this.analysis = analysis
    this
  }

  def withRunnerTransform(f: Runner => Runner): ProveEquiv = {
    transformRunner = f
    this
  }

  def runBENF(start: rise.core.Expr, goal: rise.core.Expr,
              rules: Seq[DefaultAnalysis.Rewrite]): Unit =
    runBENF(start, Seq(goal), rules)

  def runBENF(start: rise.core.Expr,
              goals: Seq[rise.core.Expr],
              rules: Seq[DefaultAnalysis.Rewrite]): Unit = {
    val normStart = BENF(Expr.fromNamed(start))
    val normGoals = goals.map(g => BENF(Expr.fromNamed(g)))
    println(s"normalized start: ${Expr.toNamed(normStart)}")
    for ((goal, i) <- normGoals.zipWithIndex) {
      println(s"normalized goal n°$i: ${Expr.toNamed(goal)}")
    }
    run(normStart, normGoals, rules)
  }

  def runCNF(start: rise.core.Expr, goal: rise.core.Expr,
             rules: Seq[DefaultAnalysis.Rewrite]): Unit =
    runCNF(start, Seq(goal), rules)

  def runCNF(start: rise.core.Expr,
             goals: Seq[rise.core.Expr],
             rules: Seq[DefaultAnalysis.Rewrite]): Unit = {
    val normStart = CNF(Expr.fromNamed(start))
    val normGoals = goals.map(g => CNF(Expr.fromNamed(g)))
    println(s"normalized start: ${Expr.toNamed(normStart)}")
    for ((goal, i) <- normGoals.zipWithIndex) {
      println(s"normalized goal n°$i: ${Expr.toNamed(goal)}")
    }
    run(normStart, normGoals, rules)
  }

  def run(start: Expr, goal: Expr,
          rules: Seq[DefaultAnalysis.Rewrite]): Unit =
    run(start, Seq(goal), rules)

  def run(start: Expr,
          goals: Seq[Expr],
          rules: Seq[DefaultAnalysis.Rewrite]): Unit = {
    val goalPatterns = goals.map(Pattern.fromExpr(_).compile())
    var remainingGoalPatterns = goalPatterns

    val egraph = EGraph.emptyWithAnalysis(analysis)
    val startId = egraph.addExpr(start)
    // val goalId = runner.egraph.addExpr(goal)
    val runner = transformRunner(Runner.init()).doneWhen { r =>
      (r.iterations.size % 3 == 0) && util.printTime("goal check", {
        remainingGoalPatterns = remainingGoalPatterns.filter(_.searchEClass(egraph, startId).isEmpty)
        remainingGoalPatterns.isEmpty
      })
      // note: could also use this to get a faster procedure,
      // but it would allow rewriting the goal as well, not just the start
      // r.egraph.findMut(startId) == r.egraph.findMut(goalId)
    }.run(egraph, filter, rules, Seq(startId))
    runner.printReport()

    if (!runner.stopReasons.contains(Done)) {
      runner.iterations.foreach(println)
      val (found, notFound) = goalPatterns.zipWithIndex.partition { case (goal, _) =>
        goal.searchEClass(egraph, startId).isDefined
      }
      println(s"found: ${found.map(_._2).mkString(", ")}")
      println(s"not found: ${notFound.map(_._2).mkString(", ")}")
      throw CouldNotProveEquiv
    }
  }
}
