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
          s"${ratio(st.goalCheckTime, st.totalTime)} goal check, "+
          s"${ratio(st.extractionTime, st.totalTime)} extraction)")
        println(s"  maximum memory ${st.memoryStats.pretty()}")
        st.beam.headOption.foreach { e =>
          println(s"  best expr:")
          println(Expr.toNamed(e))
        }
      }
    }
  }

  case class Stats(rewriteSearchTime: Long,
                   rewriteApplyTime: Long,
                   egraphRebuildTime: Long,
                   goalCheckTime: Long,
                   extractionTime: Long,
                   totalTime: Long,
                   iterations: Int,
                   egraphNodes: Int,
                   egraphClasses: Int,
                   memoryStats: util.MemoryStats,
                   beam: Seq[Expr])

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
              sketches: Seq[((Seq[Rewrite], CostFunction[_]), ExtendedPattern)]): GuidedSearch.Result =
    run(BENF, start, sketches)

  def run(normalForm: NF,
          start: rise.core.Expr,
          sketches: Seq[((Seq[Rewrite], CostFunction[_]), ExtendedPattern)]): GuidedSearch.Result = {
    val normStart = normalForm.normalize(Expr.fromNamed(start))
    println(s"normalized start: ${Expr.toNamed(normStart)}")

    // TODO: use a cheaper beamSearch returning a Boolean instead of matches?
    val beamSize = 1 // 6
    val stats = Vec.empty[GuidedSearch.Stats]

    @tailrec
    def rec(s: Int, normBeam: Seq[Expr]): Seq[Expr] = {
      if (s < sketches.length) {
        val egraph = EGraph.empty()
        val rootId = normBeam.map(egraph.addExpr)
          .reduce[EClassId] { case (a, b) => egraph.union(a, b)._1 }
        egraph.rebuild(Seq(rootId))

        println(s"---- sketch n°$s")
        val ((rules, costFunction), sketch) = sketches(s)
        // TODO: add goal check to e-graph for incremental update?
        val (growTime, runner) = util.time(transformRunner(Runner.init()).doneWhen { _ =>
          ExtendedPattern.exists(sketch, egraph, rootId)
        }.run(egraph, filter, rules, normalForm.rules, Seq(rootId)))
        val found = runner.stopReasons.contains(Done)

        val (extractionTime, matches) = if (found) {
          util.time(ExtendedPattern.beamSearch(sketch, beamSize, costFunction, egraph, rootId))
        } else {
          (0L, Seq())
        }
        val newNormBeam = matches.map { case (_, e) =>
          // FIXME: avoid normalize trick
          normalForm.normalize(ExprWithHashCons.expr(egraph)(e))
        }

        val totalIterationsTime = runner.iterations.iterator.map(_.totalTime).sum
        stats += GuidedSearch.Stats(
          rewriteSearchTime = runner.iterations.iterator.map(_.searchTime).sum,
          rewriteApplyTime = runner.iterations.iterator.map(_.applyTime).sum,
          egraphRebuildTime = runner.iterations.iterator.map(_.rebuildTime).sum,
          goalCheckTime = growTime - totalIterationsTime,
          extractionTime = extractionTime,
          totalTime = growTime + extractionTime,
          iterations = runner.iterationCount(),
          egraphNodes = runner.iterations.last.egraphNodes,
          egraphClasses = runner.iterations.last.egraphClasses,
          memoryStats = runner.iterations.iterator.map(_.memStats).reduce(_ max _),
          beam = newNormBeam
        )
        if (found) {
          assert(matches.nonEmpty)
        } else {
          runner.printReport()
          return Seq() // could not reach sketch
        }

        rec(s + 1, newNormBeam)
      } else {
        normBeam
      }
    }

    GuidedSearch.Result(rec(0, Seq(normStart)), stats)
  }
}
