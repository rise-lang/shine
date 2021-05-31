package rise.eqsat

/**
  * A way to customize how a [`Runner`] runs [`Rewrite`]s.
  * This gives you a way to prevent certain [`Rewrite`]s from exploding
  * the [`EGraph`] and dominating how much time is spent while running the [`Runner`].
  *
  * @see [[https://docs.rs/egg/0.6.0/egg/struct.RewriteScheduler.html]]
  */
trait Scheduler {
  def canSaturate(iteration: Int): Boolean
  def searchRewrite[ED, ND, TD](iteration: Int,
                                egraph: EGraph[ED, ND, TD],
                                rewrite: Rewrite[ED, ND, TD]): Vec[SearchMatches]

  // returns the number of applications
  def applyRewrite[ED, ND, TD](iteration: Int,
                               egraph: EGraph[ED, ND, TD],
                               rewrite: Rewrite[ED, ND, TD],
                               matches: Vec[SearchMatches]): Int =
    rewrite.apply(egraph, matches).size
}

object SimpleScheduler extends Scheduler {
  override def canSaturate(iteration: Int): Boolean = true

  override def searchRewrite[ED, ND, TD](iteration: Int,
                                         egraph: EGraph[ED, ND, TD],
                                         rewrite: Rewrite[ED, ND, TD]): Vec[SearchMatches] =
    rewrite.search(egraph)
}

object CuttingScheduler {
  def init(): CuttingScheduler = new CuttingScheduler(
    notApplied = HashSet.empty,
    notAppliedIteration = 0,
    currentIteration = 0,
    currentMatches = 0,
    matchLimit = 100_000
  )
}

class CuttingScheduler(var notApplied: HashSet[Object],
                       var notAppliedIteration: Int,
                       var currentIteration: Int,
                       var currentMatches: Int,
                       var matchLimit: Int) extends Scheduler {
  def withMatchLimit(limit: Int): CuttingScheduler = {
    matchLimit = limit; this
  }

  override def canSaturate(iteration: Int): Boolean =
    notApplied.isEmpty

  override def searchRewrite[ED, ND, TD](iteration: Int,
                                         egraph: EGraph[ED, ND, TD],
                                         rewrite: Rewrite[ED, ND, TD]): Vec[SearchMatches] = {
    if (iteration > currentIteration) {
      println(s"not applied: ${notApplied.map(_.asInstanceOf[Rewrite[ED, ND, TD]].name).mkString(", ")}")
      currentIteration = iteration
      currentMatches = 0
    }

    val cut = currentMatches > matchLimit
    val skip = notApplied.nonEmpty && !notApplied(rewrite)
    if (cut || skip) {
      if (notApplied.isEmpty) {
        notAppliedIteration = currentIteration
      }
      if (currentIteration == notAppliedIteration) {
        notApplied += rewrite
      }
      return Vec.empty
    }

    notApplied -= rewrite
    val matches = rewrite.search(egraph)

    def check(b: Boolean) =
      if (!b) { throw new Exception("check") }
    val totalMatches = matches.view.map(_.substs.size).sum
    var uniqueSubsts = HashSet[Object]()
    val totalSubsts = matches.view.map { m =>
      check(m.substs.size == m.substs.toSet.size)
      m.substs.view.map { s =>
        uniqueSubsts ++= s.exprs.vec
        check(s.exprs.vec.size == s.exprs.vec.toSet.size)
        uniqueSubsts ++= s.nats.vec
        check(s.nats.vec.size == s.nats.vec.toSet.size)
        uniqueSubsts ++= s.types.vec
        check(s.types.vec.size == s.types.vec.toSet.size)
        uniqueSubsts ++= s.datatypes.vec
        check(s.datatypes.vec.size == s.datatypes.vec.toSet.size)
        s.exprs.size + s.nats.size + s.types.size + s.datatypes.size
      }.sum
    }.sum
    if (uniqueSubsts.size != totalSubsts) {
      println(s"${uniqueSubsts.size} unique sustitutions in total of ${totalSubsts}")
      if (uniqueSubsts.size < 20) {
        uniqueSubsts.foreach(println)
      }
    }
    currentMatches += totalMatches

    matches
  }
}

class RuleStats(var timesApplied: Int,
                var bannedUntil: Int,
                var timesBanned: Int,
                var matchLimit: Int,
                var banLength: Int)

object BackoffScheduler {
  def init(): BackoffScheduler = new BackoffScheduler(
    defaultMatchLimit = 10_000,
    defaultBanLength = 2,
    stats = HashMap.empty,
  )
}

/** A [`Scheduler`] that implements exponential rule backoff.
  *
  * For each rewrite, there exists a configurable initial match limit.
  * If a rewrite search yield more than this limit, then we ban this
  * rule for number of iterations, double its limit, and double the time
  * it will be banned next time.
  *
  * This seems effective at preventing explosive rules like
  * associativity from taking an unfair amount of resources.
  *
  */
class BackoffScheduler(var defaultMatchLimit: Int,
                       var defaultBanLength: Int,
                       val stats: HashMap[Object, RuleStats]) extends Scheduler {
  def withInitialMatchLimit(limit: Int): BackoffScheduler = {
    defaultMatchLimit = limit; this
  }

  def withInitialBanLength(length: Int): BackoffScheduler = {
    defaultBanLength = length; this
  }

  def doNotBan[ED, ND, TD](rewrite: Rewrite[ED, ND, TD]): BackoffScheduler = {
    matchLimit(rewrite, Int.MaxValue)
  }

  def matchLimit[ED, ND, TD](rewrite: Rewrite[ED, ND, TD], limit: Int): BackoffScheduler = {
    ruleStats(rewrite).matchLimit = limit; this
  }

  def banLength[ED, ND, TD](rewrite: Rewrite[ED, ND, TD], length: Int): BackoffScheduler = {
    ruleStats(rewrite).banLength = length; this
  }

  private def ruleStats(rewrite: Object): RuleStats =
    stats.getOrElseUpdate(rewrite, {
      new RuleStats(
        timesApplied = 0,
        bannedUntil = 0,
        timesBanned = 0,
        matchLimit = defaultMatchLimit,
        banLength = defaultBanLength
      )
    })

  override def canSaturate(iteration: Int): Boolean = {
    val banned = stats.view.filter { case (_, rs) => rs.bannedUntil >= iteration }.to(Seq)
    banned.isEmpty
  }

  override def searchRewrite[ED, ND, TD](iteration: Int,
                                         egraph: EGraph[ED, ND, TD],
                                         rewrite: Rewrite[ED, ND, TD]): Vec[SearchMatches] = {
    val rs = ruleStats(rewrite)

    if (iteration < rs.bannedUntil) {
      return Vec.empty
    }

    val matches = rewrite.search(egraph)

    val totalLen = matches.view.map(_.substs.size).sum
    val threshold = rs.matchLimit << rs.timesBanned
    if (totalLen > threshold) {
      val banLength = rs.banLength + rs.timesBanned
      rs.timesBanned += 1
      rs.bannedUntil = iteration + banLength
      return Vec.empty
    }

    rs.timesApplied += 1
    matches
  }
}
