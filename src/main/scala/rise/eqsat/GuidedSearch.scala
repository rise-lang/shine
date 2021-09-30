package rise.eqsat

import scala.annotation.tailrec
import scala.language.existentials

// case class CouldNotReachSketch(i: Int, snapshot: ExtendedPattern) extends Exception

object GuidedSearch {
  // no expr = failure
  case class Result(exprs: Seq[Expr], stats: Vec[Stats]) {
    def printReport(): () = {
      stats.zipWithIndex.foreach { case (st, i) =>
        println(s"  -- sketch n°$i")
        def ratio(a: Long, b: Long) = f"${a.toDouble/b.toDouble}%.2f"
        println(s"  iterations: ${st.iterations}")
        println(s"  e-graph size: ${st.egraphNodes} nodes, ${st.egraphClasses} classes")
        println(s"  total time: ${util.prettyTime(st.totalTime)} (" +
          s"${ratio(st.rewriteSearchTime, st.totalTime)} rewrite search, " +
          s"${ratio(st.rewriteApplyTime, st.totalTime)} rewrite apply, " +
          s"${ratio(st.egraphRebuildTime, st.totalTime)} e-graph rebuild, " +
          s"${ratio(st.beamSearchTime, st.totalTime)} beam search)")
        println(s"  maximum memory ${st.memoryStats.pretty()}")
      }
    }
  }

  case class Stats(rewriteSearchTime: Long,
                   rewriteApplyTime: Long,
                   egraphRebuildTime: Long,
                   beamSearchTime: Long,
                   totalTime: Long,
                   iterations: Int,
                   egraphNodes: Int,
                   egraphClasses: Int,
                   memoryStats: util.MemoryStats)

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
              sketches: Seq[((Seq[Rewrite], CostFunction[_]), ExtendedPattern)]): GuidedSearch.Result = {
    val normStart = BENF(Expr.fromNamed(start))
    println(s"normalized start: ${Expr.toNamed(normStart)}")
    val normRules = Seq(
      RewriteDirected.Eta,
      RewriteDirected.BetaExtract,
      RewriteDirected.BetaNatExtract
    )

    val beamSize = 6
    val stats = Vec.empty[GuidedSearch.Stats]

    @tailrec
    def rec(s: Int, egraph: EGraph, rootId: EClassId): Seq[Expr] = {
      egraph.rebuild(Seq(rootId))
      println("----")
      /*
      val pcount = Analyser.init(egraph, CountProgramsUpToSize(goalSize + 10))
      pcount.analysisOf(rootId).foreach { case (size, count) =>
        println(s"programs of size ${size}: ${count}")
      }
      BeamExtract.print(3, AstSize, egraph, rootId)
      println("----")
       */

      if (s < sketches.length) {
        val ((rules, costFunction), sketch) = sketches(s)
        var matches = Seq[(_, ExprWithHashCons)]()
        // TODO: add beam analysis to e-graph for incremental update
        val (totalTime, runner) = util.time(transformRunner(Runner.init()).doneWhen { _ =>
          util.printTime("goal check", {
            matches = ExtendedPattern.beamSearch(sketch, beamSize, costFunction, egraph, rootId)
            matches.nonEmpty
          })
        }.run(egraph, filter, rules, normRules, Seq(rootId)))
        val totalIterationsTime = runner.iterations.iterator.map(_.totalTime).sum
        stats += GuidedSearch.Stats(
          rewriteSearchTime = runner.iterations.iterator.map(_.searchTime).sum,
          rewriteApplyTime = runner.iterations.iterator.map(_.applyTime).sum,
          egraphRebuildTime = runner.iterations.iterator.map(_.rebuildTime).sum,
          beamSearchTime = totalTime - totalIterationsTime,
          totalTime = totalTime,
          iterations = runner.iterationCount(),
          egraphNodes = runner.iterations.last.egraphNodes,
          egraphClasses = runner.iterations.last.egraphClasses,
          memoryStats = runner.iterations.iterator.map(_.memStats).reduce(_ max _)
        )
        if (!runner.stopReasons.contains(Done)) {
          runner.printReport()
          return Seq() // could not reach sketch
        }

        val g = EGraph.empty()
        g.hashConses = egraph.hashConses
        g.requireAnalysis(BeamExtract2(beamSize, costFunction))
        // TODO: should other known unions be restored?
        val r = matches.map { case (_, e) =>
          // FIXME: avoid BENF() trick
          val added = BENF(e, egraph.hashConses)
          println(Expr.toNamed(ExprWithHashCons.expr(g)(added)))
          g.addExpr(added)
        }.reduce[EClassId] { case (a, b) => g.union(a, b)._1 }
        rec(s + 1, g, r)
      } else {
        println(s"${egraph.nodeCount()} nodes, ${egraph.classCount()} classes")
        val ((_, costFunction), _) = sketches.last
        val beamAnalysis = egraph.getAnalysis(BeamExtract2(beamSize, costFunction))
        val bests = beamAnalysis(rootId).map(_._2)
        bests.map(ExprWithHashCons.expr(egraph))
      }
    }

    val g = EGraph.empty()
    val ((_, costFunction), _) = sketches.head
    g.requireAnalysis(BeamExtract2(beamSize, costFunction))
    GuidedSearch.Result(rec(0, g, g.addExpr(normStart)), stats)
  }
}
