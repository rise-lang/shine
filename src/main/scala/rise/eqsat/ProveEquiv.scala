package rise.eqsat

case object CouldNotProveEquiv extends Exception

object ProveEquiv {
  def init(): ProveEquiv = new ProveEquiv(
    filter = NoPredicate(),
    analysis = DefaultAnalysis,
    arrayLimit = 10,
    transformRunner = r => r,
    endRules = Seq(),
    bidirectionalSearch = false,
  )
}

class ProveEquiv(
  var filter: DefaultAnalysis.Predicate,
  var analysis: DefaultAnalysisCustomisable,
  var arrayLimit: Int,
  var transformRunner: Runner => Runner,
  var endRules: Seq[Rewrite[_, _, _]],
  var bidirectionalSearch: Boolean,
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

  def bidirectional(): ProveEquiv = {
    bidirectionalSearch = true
    this
  }

  def withEndRules(rs: Seq[Rewrite[_, _, _]]): ProveEquiv = {
    endRules = rs
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
          rules: Seq[DefaultAnalysis.Rewrite]): Unit =
    if (bidirectionalSearch) {
      runBidirectional(start, goals, rules)
    } else {
      runUnidirectional(start, goals, rules)
    }

  def runUnidirectional(start: Expr,
                        goals: Seq[Expr],
                        rules: Seq[DefaultAnalysis.Rewrite]): Unit = {
    var remainingGoals = goals

    val egraph = EGraph.emptyWithAnalysis(analysis)
    val startId = egraph.addExpr(start)
    def goalReached(g: Expr): Boolean =
      egraph.lookupExpr(g).contains(egraph.find(startId))
    val runner = transformRunner(Runner.init()).doneWhen { r =>
      util.printTime("goal check", {
        remainingGoals = remainingGoals.filterNot(goalReached)
        remainingGoals.isEmpty
      })
    }.run(egraph, filter, rules, Seq(startId))
    afterRun(runner, egraph, startId, goals, i => goalReached(goals(i)))
  }

  def runBidirectional(start: Expr,
                       goals: Seq[Expr],
                       rules: Seq[DefaultAnalysis.Rewrite]): Unit = {
    val egraph = EGraph.emptyWithAnalysis(analysis)
    val startId = egraph.addExpr(start)
    val goalIds = goals.map(egraph.addExpr)
    val runner = transformRunner(Runner.init()).doneWhen { _ =>
      goalIds.forall(g => egraph.findMut(startId) == egraph.findMut(g))
    }.run(egraph, filter, rules, Seq(startId))
    afterRun(runner, egraph, startId, goals, {
      i => egraph.findMut(startId) == egraph.findMut(goalIds(i))
    })
  }

  private def afterRun(runner: Runner, egraph: DefaultAnalysis.EGraph,
                       startId: EClassId,
                       goals: Seq[Expr], goalReached: Int => Boolean): Unit = {
    runner.printReport()

    if (!runner.stopReasons.contains(Done)) {
      // runner.iterations.foreach(println)
      val (found, notFound) = goals.indices.partition(goalReached)

      val idsToFind = notFound.map(i => egraph.addExpr(goals(i)))
      val endRunner = Runner.init().doneWhen { _ =>
        idsToFind.forall(id => egraph.findMut(startId) == egraph.findMut(id))
      }.run(egraph, NoPredicate(), endRules.asInstanceOf[Seq[DefaultAnalysis.Rewrite]], Seq(startId))
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
