package rise.eqsat

import scala.annotation.tailrec
import scala.language.existentials

case class CouldNotReachSketch(i: Int, snapshot: ExtendedPattern) extends Exception

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
              sketches: Seq[((Seq[Rewrite], CostFunction[_]), ExtendedPattern)]): Expr = {
    val normStart = BENF(Expr.fromNamed(start))
    println(s"normalized start: ${Expr.toNamed(normStart)}")
    val normRules = Seq(
      RewriteDirected.Eta,
      RewriteDirected.BetaExtract,
      RewriteDirected.BetaNatExtract
    )

    val beamSize = 6

    @tailrec
    def rec(s: Int, egraph: EGraph, rootId: EClassId): Expr = {
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
        val runner = transformRunner(Runner.init()).doneWhen { r =>
          util.printTime("goal check", {
            matches = ExtendedPattern.beamSearch(sketch, beamSize, costFunction, egraph, rootId)
            matches.nonEmpty
          })
        }.run(egraph, filter, rules, normRules, Seq(rootId))
        runner.printReport()
        if (!runner.stopReasons.contains(Done)) {
          throw CouldNotReachSketch(s, sketch)
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
        val best = beamAnalysis(rootId).head._2
        ExprWithHashCons.expr(egraph)(best)
      }
    }

    val g = EGraph.empty()
    val ((_, costFunction), _) = sketches.head
    g.requireAnalysis(BeamExtract2(beamSize, costFunction))
    rec(0, g, g.addExpr(normStart))
  }
}
