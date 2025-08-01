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
case class MemoryLimit(bytes: Long) extends StopReason {
  override def toString: String = s"MemoryLimit(${util.prettyMem(bytes)})"
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
    memoryLimit = 2L * 1024L * 1024L * 1024L, // 2GiB
    scheduler = SimpleScheduler,
    totalRemoved = 0L
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
             var memoryLimit: Long,
             var scheduler: Scheduler,
             var totalRemoved: Long) {
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

  def withMemoryLimit(limit: Long): Runner = {
    memoryLimit = limit; this
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
    val memStats = iterations.iterator.map(_.memStats).reduce(_ max _)
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
    println(s"  Maximum Memory: ${memStats.pretty()}")
  }

  def run(egraph: EGraph,
          filter: Predicate,
          rules: Seq[Rewrite],
          normRules: Seq[RewriteDirected],
          roots: Seq[EClassId]): Runner = {
    egraph.rebuild(roots)
    egraph.requireAnalyses(filter.requiredAnalyses())
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
      nRebuilds = 0,
      memStats = util.memStats()
    )
    // println(iteration0)
    iterations += iteration0

    def end(): Runner = {
      println(s"nodes removed by directed rewriting: $totalRemoved")
      egraph.releaseAnalyses(filter.requiredAnalyses())
      rules.foreach(r => egraph.releaseAnalyses(r.requiredAnalyses()))
      normRules.foreach(r => egraph.releaseAnalyses(r.requiredAnalyses()))
      this
    }

    val startTime = System.nanoTime()
    while (true) {
      if (done(this)) { // TODO: record runtime of 'done' in Iteration
        stopReasons += Done
      }
      if (stopReasons.nonEmpty) { return end() }

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
      if (iter.memStats.used > memoryLimit) {
        stopReasons += MemoryLimit(iter.memStats.used)
      }
      if (iter.egraphNodes > nodeLimit) {
        stopReasons += NodeLimit(iter.egraphNodes)
      }
      if (iterationCount() >= iterLimit) {
        stopReasons += IterationLimit(iterationCount())
      }
    }

    end()
  }

  // TODO: could check limits in-between searches and matches like in egg
  private def runOne(egraph: EGraph,
                     roots: Seq[EClassId],
                     filter: Predicate,
                     rules: Seq[Rewrite],
                     normRules: Seq[RewriteDirected]): Iteration = {
    val time0 = System.nanoTime()
    val i = iterations.size
    val shc = SubstsVM

    val matches = rules.map { r =>
      scheduler.searchRewrite(i, egraph, shc, r)
    }
    val normMatches = normRules.map { nr => nr.search(egraph) }

    val memStats1 = util.memStats()
    val time1 = System.nanoTime()

    val applied = HashMap.empty[String, Int]
    def updateApplied(name: String, newlyApplied: Int): Unit = {
      if (newlyApplied > 0) {
        applied.updateWith(name) {
          case Some(count) => Some(count + newlyApplied)
          case None => Some(newlyApplied)
        }
      }
    }

    rules.zip(matches).foreach { case (r, ms) =>
      // TODO: extract Substs as proper maps or vecmaps during rewrite application?
      //  for faster access and local inserts
      val newlyApplied = scheduler.applyRewrite(i, egraph, shc, r)(ms)
      updateApplied(r.name, newlyApplied)
    }
    val withAlternative = Vec.empty[RewriteDirected.Match]
    normRules.zip(normMatches).foreach { case (nr, ms) =>
      var newlyApplied = 0
      ms.foreach { case (m, applier) =>
        val (createsAlt, unionAlt) = applier()
        if (unionAlt) { newlyApplied += 1 }
        if (createsAlt) { withAlternative += m }
      }
      updateApplied(nr.name, newlyApplied)
    }

    val time2 = System.nanoTime()

    // TODO: record removeTime in Iteration
    // val (removeTime, removed) = util.time {
    val removed =
      RewriteDirected.greedyRemoval(egraph, withAlternative)
    totalRemoved += removed

    val nRebuilds = egraph.rebuild(roots, filter)

    val memStats3 = util.memStats()
    val time3 = System.nanoTime()

    new Iteration(
      egraphNodes = egraph.nodeCount(),
      egraphClasses = egraph.classCount(),
      memoSize = egraph.memo.size,
      applied = applied,
      searchTime = time1 - time0,
      applyTime = time2 - time1,
      rebuildTime = time3 - time2,
      totalTime = time3 - time0,
      nRebuilds = nRebuilds,
      memStats = memStats1 max memStats3
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
                val nRebuilds: Int,
                val memStats: util.MemoryStats) {
  override def toString: String = {
    s"Iteration:\n" +
    s"  #nodes: $egraphNodes, " +
      s"#classes: $egraphClasses, " +
      s"#memo: $memoSize, " +
      s"search: ${util.prettyTime(searchTime)}, " +
      s"apply: ${util.prettyTime(applyTime)}, " +
      s"rebuild: ${util.prettyTime(rebuildTime)}, " +
      s"total: ${util.prettyTime(totalTime)}, " +
      s"#rebuilds: $nRebuilds\n" +
    s"  applied: $applied\n" +
    s"  memory ${memStats.pretty()}"
  }
}
