package rise.eqsat

import java.time.Duration
import scala.annotation.tailrec

sealed trait StopReason
case object Saturated extends StopReason
case class IterationLimit(iterations: Int) extends StopReason
case class NodeLimit(nodes: Int) extends StopReason
case class TimeLimit(duration: Long) extends StopReason {
  override def toString: String = s"TimeLimit(${util.prettyTime(duration)})"
}
case object Done extends StopReason

object Runner {
  def init(): Runner = new Runner(
    iterations = Vec(),
    stopReasons = Vec(),
    done = _ => false,

    iterLimit = 60,
    nodeLimit = 600_000,
    timeLimit = Duration.ofSeconds(30).toNanos,
    scheduler = SimpleScheduler
  )
}

/** Facilitates searching for rewrites using an [[Egraph]].
  * This technique is called "equality saturation" in general.
  * @see [[https://docs.rs/egg/0.6.0/egg/struct.Runner.html]]
  */
class Runner(var iterations: Vec[Iteration],
             var stopReasons: Vec[StopReason],
             var done: Runner => Boolean,
             var iterLimit: Int,
             var nodeLimit: Int,
             var timeLimit: Long,
             var scheduler: Scheduler) {
  def iterationCount(): Int =
    iterations.size - 1

  def withIterationLimit(limit: Int): Runner = {
    iterLimit = limit; this
  }

  def withNodeLimit(limit: Int): Runner = {
    nodeLimit = limit; this
  }

  def withTimeLimit(limit: Duration): Runner = {
    timeLimit = limit.toNanos; this
  }

  def withScheduler(sched: Scheduler): Runner = {
    scheduler = sched; this
  }

  def doneWhen(f: Runner => Boolean): Runner = {
    done = f; this
  }

  def printReport(): Unit = {
    val searchTime = iterations.iterator.map(_.searchTime).sum
    val applyTime = iterations.iterator.map(_.applyTime).sum
    val rebuildTime = iterations.iterator.map(_.rebuildTime).sum
    val totalTime = iterations.iterator.map(_.totalTime).sum
    val nRebuilds = iterations.iterator.map(_.nRebuilds.toLong).sum
    println("-- Runner report --")
    println(s"  Stop reasons: ${stopReasons.mkString(", ")}")
    println(s"  Iterations: ${iterationCount()}")
    val nodes = iterations.last.egraphNodes
    val classes = iterations.last.egraphClasses
    val memo = iterations.last.memoSize
    println(s"  EGraph size: $nodes nodes, $classes classes, $memo memo")
    def ratio(a: Double, b: Double) = f"${a/b}%.2f"
    println(s"  Rebuilds: $nRebuilds, " +
      s"${ratio(nRebuilds.toDouble, iterationCount().toDouble)} per iter")
    println(s"  Total time: ${util.prettyTime(totalTime)} (" +
      s"${ratio(searchTime.toDouble, totalTime.toDouble)} search, " +
      s"${ratio(applyTime.toDouble, totalTime.toDouble)} apply, " +
      s"${ratio(rebuildTime.toDouble, totalTime.toDouble)} rebuild)")
  }

  def run(egraph: EGraph,
          filter: Predicate,
          rules: Seq[Rewrite],
          normRules: Seq[RewriteDirected],
          roots: Seq[EClassId]): Runner = {
    egraph.rebuild(roots)
    rules.foreach(r => egraph.requireAnalyses(r.requiredAnalyses()))
    normRules.foreach(r => egraph.requireAnalyses(r.requiredAnalyses()))

    val iteration0 = new Iteration(
      egraphNodes = egraph.nodeCount(),
      egraphClasses = egraph.classCount(),
      memoSize = egraph.memo.size,
      applied = HashMap.empty,
      searchTime = 0,
      applyTime = 0,
      rebuildTime = 0,
      totalTime = 0,
      nRebuilds = 0
    )
    println(iteration0)
    iterations += iteration0

    val startTime = System.nanoTime()
    while (true) {
      if (done(this)) {
        stopReasons += Done
      }
      if (stopReasons.nonEmpty) { return this }

      val iter = runOne(egraph, roots, filter, rules, normRules)
      println(iter)

      if (iter.applied.isEmpty &&
        scheduler.canSaturate(iterations.size)) {
        stopReasons += Saturated
      }

      iterations += iter

      val elapsed = System.nanoTime() - startTime
      if (elapsed > timeLimit) {
        stopReasons += TimeLimit(elapsed)
      }
      if (iter.egraphNodes > nodeLimit) {
        stopReasons += NodeLimit(iter.egraphNodes)
      }
      if (iterationCount() >= iterLimit) {
        stopReasons += IterationLimit(iterationCount())
      }
    }

    rules.foreach(r => egraph.releaseAnalyses(r.requiredAnalyses()))
    normRules.foreach(r => egraph.releaseAnalyses(r.requiredAnalyses()))
    this
  }

  // TODO: could check limits in-between searches and matches like in egg
  private def runOne(egraph: EGraph,
                     roots: Seq[EClassId],
                     filter: Predicate,
                     rules: Seq[Rewrite],
                     normRules: Seq[RewriteDirected]): Iteration = {
    val time0 = System.nanoTime()
    val i = iterations.size
    val shc = SubstHashCons.empty

    val matches = rules.map { r =>
      scheduler.searchRewrite(i, egraph, shc, r)
    }

    val time1 = System.nanoTime()

    val applied = HashMap.empty[String, Int]
    rules.zip(matches).map { case (r, ms) =>
      // TODO: extract Substs as proper maps or vecmaps during rewrite application?
      //  for faster access and local inserts
      val newlyApplied = scheduler.applyRewrite(i, egraph, shc, r, ms)
      if (newlyApplied > 0) {
        applied.updateWith(r.name) {
          case Some(count) => Some(count + newlyApplied)
          case None => Some(newlyApplied)
        }
      }
    }

    val time2 = System.nanoTime()

    val nRebuilds = egraph.rebuild(roots, filter)

    val time3 = System.nanoTime()

    // TODO: include in Iteration
    util.printTime("norm time", {
      // TODO:
      // val applied = HashMap.empty[String, Int]
      var applied = 0
      var mayRemove = 0

      @tailrec
      def rec(): Unit = {
        // avoid deleting overlapping matches
        val alreadyMatched = HashSet.empty[(EClassId, ENode)]
        val res = normRules.flatMap { nr => nr.search(egraph) }
          .filter { case (m, applier) =>
            if (RewriteDirected.overlapsAlreadyMatched(alreadyMatched, m)) {
              false
            } else {
              val addedSomething = applier()
              applied += 1
              if (addedSomething) {
                mayRemove += 1
                RewriteDirected.extendAlreadyMatched(alreadyMatched, m)
              }
              addedSomething
            }
          }
        val removed = RewriteDirected.greedyRemoval(egraph, res)
        println(s"removed: $removed")
        egraph.rebuild(roots, filter)
        if (removed > 0) {
          rec()
        }
      }

      rec()

      println(s"applied: $applied, may remove: $mayRemove")
    })

    new Iteration(
      egraphNodes = egraph.nodeCount(),
      egraphClasses = egraph.classCount(),
      memoSize = egraph.memo.size,
      applied = applied,
      searchTime = time1 - time0,
      applyTime = time2 - time1,
      rebuildTime = time3 - time2,
      totalTime = time3 - time0,
      nRebuilds = nRebuilds
    )
  }
}

class Iteration(val egraphNodes: Int,
                val egraphClasses: Int,
                val memoSize: Int,
                // map from rule name to number of times it was newly applied
                val applied: HashMap[String, Int],
                val searchTime: Long,
                val applyTime: Long,
                val rebuildTime: Long,
                val totalTime: Long,
                val nRebuilds: Int) {
  override def toString: String = {
    s"Iteration(#nodes: $egraphNodes, " +
      s"#classes: $egraphClasses, " +
      s"#memo: $memoSize, " +
      s"applied: $applied, " +
      s"search: ${util.prettyTime(searchTime)}, " +
      s"apply: ${util.prettyTime(applyTime)}, " +
      s"rebuild: ${util.prettyTime(rebuildTime)}, " +
      s"total: ${util.prettyTime(totalTime)}, " +
      s"#rebuilds: $nRebuilds)"
  }
}
