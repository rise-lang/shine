package rise.eqsat

import java.time.Duration

sealed trait StopReason
case object Saturated extends StopReason
case class IterationLimit(iterations: Int) extends StopReason
case class NodeLimit(nodes: Int) extends StopReason
case class TimeLimit(duration: Duration) extends StopReason
case object Done extends StopReason

object Runner {
  def withAnalysis[D](analysis: Analysis[D]): Runner[D] = new Runner(
    egraph = EGraph.emptyWithAnalysis(analysis),
    iterations = Vec(),
    stopReasons = Vec(),
    done = _ => false,

    iterLimit = 30,
    nodeLimit = 10_000,
    timeLimit = Duration.ofSeconds(5).toNanos,
    startTime = None
  )
}

class Runner[D](var egraph: EGraph[D],
                var iterations: Vec[Iteration],
                var stopReasons: Vec[StopReason],
                var done: Runner[D] => Boolean,
                var iterLimit: Int,
                var nodeLimit: Int,
                var timeLimit: Long,
                var startTime: Option[Long]) {
  def withIterationLimit(limit: Int): Runner[D] = {
    iterLimit = limit; this
  }

  def withNodeLimit(limit: Int): Runner[D] = {
    nodeLimit = limit; this
  }

  def withTimeLimit(limit: Duration): Runner[D] = {
    timeLimit = limit.toNanos; this
  }

  def doneWhen(f: Runner[D] => Boolean): Runner[D] = {
    done = f; this
  }

  def printReport(): Unit = {
    val searchTime = iterations.iterator.map(_.searchTime).sum
    val applyTime = iterations.map(_.searchTime).sum
    val rebuildTime = iterations.map(_.searchTime).sum
    val totalTime = iterations.map(_.searchTime).sum
    val nRebuilds = iterations.map(_.nRebuilds.toLong).sum
    val nodes = iterations.last.egraphNodes
    val classes = iterations.last.egraphClasses
    println("-- Runner report --")
    println(s"  Stop reasons: $stopReasons")
    println(s"  Iterations: ${iterations.size}")
    println(s"  EGraph size: ${nodes} nodes, ${classes} classes, ${egraph.memo.size} memo")
    def ratio(a: Double, b: Double) = f"${a/b}%.2f"
    println(s"  Rebuilds: ${nRebuilds}, " +
      s"${ratio(nRebuilds.toDouble, iterations.size.toDouble)} per iter")
    // FIXME: this prints garbage
    println(s"  Total time: ${Duration.ofNanos(totalTime)} (" +
      s"${ratio(searchTime.toDouble, totalTime.toDouble)} search, " +
      s"${ratio(applyTime.toDouble, totalTime.toDouble)} apply, " +
      s"${ratio(rebuildTime.toDouble, totalTime.toDouble)} rebuild)")
  }

  def run(rules: Seq[Rewrite[D]]): Runner[D] = {
    egraph.rebuild()

    startTime = Some(System.nanoTime())
    while (true) {
      if (done(this)) {
        stopReasons += Done
      }
      if (stopReasons.nonEmpty) { return this }

      val iter = runOne(rules)
      iterations += iter

      if (iter.nRewrites == 0) {
        stopReasons += Saturated
      }
      if (iter.totalTime > timeLimit) {
        stopReasons += TimeLimit(Duration.ofNanos(iter.totalTime))
      }
      if (iter.egraphNodes > nodeLimit) {
        stopReasons += NodeLimit(iter.egraphNodes)
      }
      if (iterations.size >= iterLimit) {
        stopReasons += IterationLimit(iterations.size)
      }
    }

    this
  }

  private def runOne(rules: Seq[Rewrite[D]]): Iteration = {
    val time0 = System.nanoTime()

    val matches = rules.map { r => r.search(egraph) }

    val time1 = System.nanoTime()

    val nRewrites = rules.zip(matches).map { case (r, ms) =>
      r.apply(egraph, ms).size
    }.sum

    val time2 = System.nanoTime()

    val nRebuilds = egraph.rebuild();

    val time3 = System.nanoTime()

    new Iteration(
      egraphNodes = egraph.nodeCount(),
      egraphClasses = egraph.classCount(),
      searchTime = time1 - time0,
      applyTime = time2 - time1,
      rebuildTime = time3 - time2,
      totalTime = time3 - time0,
      nRewrites = nRewrites,
      nRebuilds = nRebuilds
    )
  }
}

class Iteration(val egraphNodes: Int,
                val egraphClasses: Int,
                val searchTime: Long,
                val applyTime: Long,
                val rebuildTime: Long,
                val totalTime: Long,
                val nRewrites: Int,
                val nRebuilds: Int)