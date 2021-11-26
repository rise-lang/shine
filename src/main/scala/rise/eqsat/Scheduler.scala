package rise.eqsat

import scala.util.Random

/**
  * A way to customize how a [`Runner`] runs [`Rewrite`]s.
  * This gives you a way to prevent certain [`Rewrite`]s from exploding
  * the [`EGraph`] and dominating how much time is spent while running the [`Runner`].
  *
  * @see [[https://docs.rs/egg/0.6.0/egg/struct.RewriteScheduler.html]]
  */
trait Scheduler {
  def canSaturate(iteration: Int): Boolean
  def searchRewrite(iteration: Int,
                    egraph: EGraph,
                    shc: Substs,
                    rewrite: Rewrite): Vec[SearchMatches[shc.Subst]]

  // returns the number of applications
  def applyRewrite(iteration: Int,
                   egraph: EGraph,
                   shc: Substs,
                   rewrite: Rewrite)(
                   matches: Vec[SearchMatches[shc.Subst]]): Int =
    rewrite.apply(egraph, shc)(matches).size
}

object SimpleScheduler extends Scheduler {
  override def canSaturate(iteration: Int): Boolean = true

  override def searchRewrite(iteration: Int,
                             egraph: EGraph,
                             shc: Substs,
                             rewrite: Rewrite): Vec[SearchMatches[shc.Subst]] =
    rewrite.search(egraph, shc)
}

/* TODO: only for SubstHC
object CuttingScheduler {
  def init(): CuttingScheduler = new CuttingScheduler(
    notApplied = HashSet.empty,
    notAppliedIteration = 0,
    currentIteration = 0,
    hashConsSizeLimit = 100_000
  )
}

class CuttingScheduler(var notApplied: HashSet[Object],
                       var notAppliedIteration: Int,
                       var currentIteration: Int,
                       var hashConsSizeLimit: Int) extends Scheduler {
  def withHashConsSizeLimit(limit: Int): CuttingScheduler = {
    hashConsSizeLimit = limit; this
  }

  override def canSaturate(iteration: Int): Boolean =
    notApplied.isEmpty

  override def searchRewrite(iteration: Int,
                             egraph: EGraph,
                             shc: Substs,
                             rewrite: Rewrite): Vec[SearchMatches[shc.Subst]] = {
    if (iteration > currentIteration) {
      println(s"not applied: ${notApplied.map(_.asInstanceOf[Rewrite].name).mkString(", ")}")
      currentIteration = iteration
    }

    val currentHashConsSize =
      shc.exprs.memo.size + shc.nats.memo.size + shc.types.memo.size + shc.dataTypes.memo.size
    val cut = currentHashConsSize > hashConsSizeLimit
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
    val matches = rewrite.search(egraph, shc)
    def check(b: Boolean) =
      if (!b) { throw new Exception("check") }
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

    matches
  }
}
*/

object SamplingScheduler {
  def init(): SamplingScheduler = new SamplingScheduler(
    defaultLimit = 1_000,
    limits = HashMap.empty,
    lastSampledIteration = -1,
    random = new Random
  )
  
  def initWithSeed(seed: Int): SamplingScheduler = new SamplingScheduler(
    defaultLimit = 1_000,
    limits = HashMap.empty,
    lastSampledIteration = -1,
    random = new Random(seed)
  )
}

/** A [`Scheduler`] that implements rule sampling.
  *
  * For each rewrite, there exists a configurable match limit.
  * If a rewrite search yield more than this limit,
  * random match samples will be kept, and the rest discarded.
  *
  */
class SamplingScheduler(var defaultLimit: Int,
                        var limits: HashMap[Rewrite, Int],
                        var lastSampledIteration: Int,
                        var random: Random)
extends Scheduler {
  def withDefaultLimit(l: Int): SamplingScheduler = {
    this.defaultLimit = l; this
  }

  override def canSaturate(iteration: Int): Boolean = iteration > lastSampledIteration

  override def searchRewrite(iteration: Int, egraph: EGraph, shc: Substs, rewrite: Rewrite): Vec[SearchMatches[shc.Subst]] = {
    val limit = limits.getOrElse(rewrite, defaultLimit)
    val matches = rewrite.search(egraph, shc)
    if (matches.size > limit) {
      println(s"sampled $limit from ${matches.size} matches")
      random.shuffle(matches).take(limit)
    } else {
      matches
    }
  }
}

class RuleStats(var timesApplied: Int,
                var bannedUntil: Int,
                var timesBanned: Int,
                var matchLimit: Int,
                var banLength: Int)

object BackoffScheduler {
  def init(): BackoffScheduler = new BackoffScheduler(
    defaultMatchLimit = 1_000,
    defaultBanLength = 5,
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
                       val stats: HashMap[Rewrite, RuleStats]) extends Scheduler {
  def withInitialMatchLimit(limit: Int): BackoffScheduler = {
    defaultMatchLimit = limit; this
  }

  def withInitialBanLength(length: Int): BackoffScheduler = {
    defaultBanLength = length; this
  }

  def doNotBan(rewrite: Rewrite): BackoffScheduler = {
    matchLimit(rewrite, Int.MaxValue)
  }

  def matchLimit(rewrite: Rewrite, limit: Int): BackoffScheduler = {
    ruleStats(rewrite).matchLimit = limit; this
  }

  def banLength(rewrite: Rewrite, length: Int): BackoffScheduler = {
    ruleStats(rewrite).banLength = length; this
  }

  private def ruleStats(rewrite: Rewrite): RuleStats =
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
    val banned = stats.view.filter { case (_, rs) => rs.bannedUntil > iteration }.to(Seq)
    if (banned.nonEmpty) {
      val minBan = banned.map { case (_, s) => s.bannedUntil }.min
      assert(minBan >= iteration)
      val delta = minBan - iteration

      for ((_, s) <- banned) {
        s.bannedUntil -= delta
      }
    }
    banned.isEmpty
  }

  override def searchRewrite(iteration: Int,
                             egraph: EGraph,
                             shc: Substs,
                             rewrite: Rewrite): Vec[SearchMatches[shc.Subst]] = {
    val rs = ruleStats(rewrite)

    if (iteration < rs.bannedUntil) {
      return Vec.empty
    }

    val matches = rewrite.search(egraph, shc)

    val totalLen = matches.view.map(_.substs.size).sum
    val threshold = rs.matchLimit << rs.timesBanned
    if (totalLen > threshold) {
      val banLength = rs.banLength << rs.timesBanned
      rs.timesBanned += 1
      rs.bannedUntil = iteration + banLength
      return Vec.empty
    }

    rs.timesApplied += 1
    matches
  }
}
