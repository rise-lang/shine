package rise.eqsat

import scala.annotation.tailrec
import scala.language.existentials

object GuidedSearch {
  object Step {
    def init(nf: NF): Step = Step(nf, Seq(), SketchAny(TypePatternAny), BeamExtractor(1, AstSize))
  }

  case class Step(normalForm: NF, rules: Seq[Rewrite], sketch: Sketch, extractor: Extractor) {
    def withNormalForm(nf: NF): Step =
      this.copy(normalForm = nf)

    def withRules(rs: Seq[Rewrite]): Step =
      this.copy(rules = rs)

    def withSketch(s: Sketch): Step =
      this.copy(sketch = s)

    def withExtractor(ex: Extractor): Step =
      this.copy(extractor = ex)

    def compose(other: Step): Step = {
      assert(normalForm == other.normalForm)
      val mergedRules = (rules ++ other.rules).distinctBy(_.name)
      Step(normalForm, mergedRules, other.sketch, other.extractor)
    }
  }

  trait Extractor {
    def extract(sketch: Sketch, egraph: EGraph, id: EClassId): Seq[Expr]
  }

  // TODO: accept normal form
  case class BeamExtractor(beamSize: Int, costFunction: CostFunction[_]) extends Extractor {
    override def extract(sketch: Sketch, egraph: EGraph, id: EClassId): Seq[Expr] =
      Sketch.beamSearch(sketch, beamSize, costFunction, egraph, id)
        .map { case (_, e) => ExprWithHashCons.expr(egraph)(e) }
  }

  // no expr = failure
  case class Result(exprs: Seq[Expr], stats: Vec[Stats]) {
    def printReport(): () = {
      stats.zipWithIndex.foreach { case (st, i) =>
        println(s"  -- step n°$i")
        def ratio(a: Long, b: Long) = f"${a.toDouble/b.toDouble}%.2f"
        println(s"  iterations: ${st.iterations}, rewrites: ${st.rewriteCount}, nf rewrites: ${st.normRewriteCount}")
        println(s"e-graph size: ${st.egraphNodes} nodes, ${st.egraphClasses} classes")
        println(s"  total time: ${util.prettyTime(st.totalTime)} (" +
          s"${ratio(st.initializeTime, st.totalTime)} initialize, " +
          s"${ratio(st.rewriteSearchTime, st.totalTime)} rewrite search, " +
          s"${ratio(st.rewriteApplyTime, st.totalTime)} rewrite apply, " +
          s"${ratio(st.egraphRebuildTime, st.totalTime)} e-graph rebuild, " +
          s"${ratio(st.goalCheckTime, st.totalTime)} goal check, "+
          s"${ratio(st.extractionTime, st.totalTime)} extraction)")
        println(s"  maximum memory ${st.memoryStats.pretty()}")
        // if (!stats.lift(i + 1).exists(_.beam.nonEmpty)) {
          st.beam.headOption.foreach { e =>
            println(s"  best expr:")
            println(Expr.toNamed(e))
            // util.dotPrintTmp(s"best_step${i}_", Expr.toNamed(e))
          }
        // }
      }
    }
  }

  case class Stats(initializeTime: Long,
                   rewriteSearchTime: Long,
                   rewriteApplyTime: Long,
                   egraphRebuildTime: Long,
                   goalCheckTime: Long,
                   extractionTime: Long,
                   totalTime: Long,
                   iterations: Int,
                   normRewriteCount: Long,
                   rewriteCount: Long,
                   egraphNodes: Int,
                   egraphClasses: Int,
                   memoryStats: util.MemoryStats,
                   beam: Seq[Expr])

  def init(): GuidedSearch = new GuidedSearch(
    filter = NoPredicate(),
    transformRunner = r => r,
    verbose = false,
  )
}

class GuidedSearch(
  var filter: Predicate,
  var transformRunner: Runner => Runner,
  var verbose: Boolean,
) {
  def withFilter(filter: Predicate): GuidedSearch = {
    this.filter = filter
    this
  }

  def withRunnerTransform(f: Runner => Runner): GuidedSearch = {
    transformRunner = f
    this
  }

  def withVerbose(v: Boolean): GuidedSearch = {
    verbose = v
    this
  }

  def run(start: rise.core.Expr, steps: Seq[GuidedSearch.Step]): GuidedSearch.Result =
    run(Expr.fromNamed(start), steps)

  def run(start: Expr, steps: Seq[GuidedSearch.Step]): GuidedSearch.Result = {
    val stats = Vec.empty[GuidedSearch.Stats]

    val startTime = System.nanoTime()
    // note: this is a bit hacky
    val timeLimit = transformRunner(Runner.init()).timeLimit

    @tailrec
    def rec(s: Int, beam: Seq[Expr]): Seq[Expr] = {
      if (s < steps.length) {
        if (verbose) {
          println(s"---- step n°$s")
        }
        val step = steps(s)

        var normRewriteCount = 0L
        val (initializeTime, (egraph, rootId)) = util.time{
          val egraph = EGraph.empty()
          val normBeam = beam.map { e =>
            val (n, rc) = step.normalForm.normalizeCountRewrites(e)
            normRewriteCount += rc
            n
          }
          if (verbose) {
            println(s"beam head: ${Expr.toNamed(normBeam.head)}")
          }
          val rootId = normBeam.map(egraph.addExpr)
            .reduce[EClassId] { case (a, b) => egraph.union(a, b)._1 }
          egraph.rebuild(Seq(rootId))
          (egraph, rootId)
        }

        // TODO: add goal check to e-graph for incremental update?
        val mergedRules = (step.rules ++ step.normalForm.rules).distinctBy(_.name)
        val (growTime, runner) = util.time(
          transformRunner(Runner.init().withVerbose(verbose))
          // note: update time limit
          .withTimeLimit(java.time.Duration.ofNanos(timeLimit - (System.nanoTime() - startTime)))
          .doneWhen { _ =>
            util.printTime("goal check", Sketch.exists(step.sketch, egraph, rootId))
          }.run(egraph, filter, mergedRules, Seq(), Seq(rootId)))
        val found = runner.stopReasons.contains(Done)

        val (extractionTime, newBeam) = if (found) {
          util.time(step.extractor.extract(step.sketch, egraph, rootId))
        } else {
          (0L, Seq())
        }

        val totalIterationsTime = runner.iterations.iterator.map(_.totalTime).sum
        stats += GuidedSearch.Stats(
          initializeTime = initializeTime,
          rewriteSearchTime = runner.iterations.iterator.map(_.searchTime).sum,
          rewriteApplyTime = runner.iterations.iterator.map(_.applyTime).sum,
          egraphRebuildTime = runner.iterations.iterator.map(_.rebuildTime).sum,
          goalCheckTime = growTime - totalIterationsTime,
          extractionTime = extractionTime,
          totalTime = initializeTime + growTime + extractionTime,
          iterations = runner.iterationCount(),
          normRewriteCount = normRewriteCount,
          rewriteCount = runner.iterations.map(_.applied.values.map(_.toLong).sum).sum,
          egraphNodes = runner.iterations.last.egraphNodes,
          egraphClasses = runner.iterations.last.egraphClasses,
          memoryStats = runner.iterations.iterator.map(_.memStats).reduce(_ max _),
          beam = newBeam
        )
        if (found) {
          assert(newBeam.nonEmpty)
        } else {
          runner.printReport()
          if (!verbose) {
            runner.iterations.foreach(println)
          }
          return Seq() // could not reach sketch
        }

        rec(s + 1, newBeam)
      } else {
        // FIXME: avoid normalizing here, extract normalized terms instead?
        beam.map(steps.last.normalForm.normalize)
      }
    }

    GuidedSearch.Result(rec(0, Seq(start)), stats)
  }
}
