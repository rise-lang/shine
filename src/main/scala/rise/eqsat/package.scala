package rise

import scala.collection.mutable

/** Equality Saturation for Rise,
  * based on the [[https://egraphs-good.github.io/ `egg` library]].
  */
package object eqsat {
  type ENode = Node[EClassId, NatId, DataTypeId, Address]
  type PNode = Node[Pattern, NatPattern, DataTypePattern, AddressPattern]
  type Address = AddressNode

  /** A key to identify [[EClass]]es within an [[EGraph]] */
  case class EClassId(i: Int)

  /** A key to identify interned nats */
  case class NatId(i: Int)

  /** A key to identify interned types */
  sealed trait TypeId
  /** A key to identify interned data types */
  case class DataTypeId(i: Int) extends TypeId
  /** A key to identify interned types which are not data types */
  case class NotDataTypeId(i: Int) extends TypeId

  type Vec[T] = mutable.ArrayBuffer[T]
  val Vec: mutable.ArrayBuffer.type = mutable.ArrayBuffer
  type HashMap[K, V] = mutable.HashMap[K, V]
  val HashMap: mutable.HashMap.type = mutable.HashMap
  type HashSet[V] = mutable.HashSet[V]
  val HashSet: mutable.HashSet.type = mutable.HashSet

  /** Normal Form. */
  trait NF {
    val normalize: Expr => Expr
    val normalizeCountRewrites: Expr => (Expr, Long)
    val rules: Seq[Rewrite]
    val directedRules: Seq[RewriteDirected]
  }

  object BENF extends NF {
    val extractAnalysis: SmallestCostAnalysis[(BENFRedexCount.Data, Int)] =
      SmallestCostAnalysis(LexicographicCost(BENFRedexCount(), AstSize))

    override val normalize: Expr => Expr = e => BENF(e)._1
    override val normalizeCountRewrites: Expr => (Expr, Long) = BENF
    override val rules: Seq[Rewrite] = {
      import eqsat.rules._
      Seq(eta, betaExtract, betaNatExtract)
    }
    override val directedRules: Seq[RewriteDirected] = Seq(
      RewriteDirected.Eta,
      RewriteDirected.BetaExtract,
      RewriteDirected.BetaNatExtract
    )
  }

  object CNF extends NF {
    override val normalize: Expr => Expr = CNF
    override lazy val normalizeCountRewrites: Expr => (Expr, Long) = ???
    override val rules: Seq[Rewrite] = {
      import eqsat.rules._
      Seq(
        eta, betaExtract, betaNatExtract,
        combinatory.compositionIntro, combinatory.compositionAssoc2,
      )
    }
    override val directedRules: Seq[RewriteDirected] = Seq(
      RewriteDirected.Eta,
      RewriteDirected.BetaExtract,
      RewriteDirected.BetaNatExtract,
      RewriteDirected.CompositionIntro,
      RewriteDirected.CompositionAssoc2
    )
  }

  def BENF(e: Expr): (Expr, Long) = {
    val egraph = EGraph.empty()
    val id = egraph.addExpr(e)
    val (n, r) = BENF_internal(egraph, id)
    (ExprWithHashCons.expr(egraph)(n), r)
  }
  def BENF(e: ExprWithHashCons, hc: HashConses): (ExprWithHashCons, Long) = {
    val egraph = EGraph.empty()
    egraph.hashConses = hc
    val id = egraph.addExpr(e)
    BENF_internal(egraph, id)
  }

  private def BENF_internal(egraph: EGraph, id: EClassId): (ExprWithHashCons, Long) = {
    val rewrites = Runner.init().run(egraph, NoPredicate(), Seq(), BENF.directedRules, Seq(id))
      .iterations.map(_.applied.values.map(_.toLong).sum).sum
    val (normalized, _) = Extractor.findBestOf(egraph, BENFRedexCount(), id)
    (normalized, rewrites)
  }

  // Combinator Normal Form
  def CNF(e: Expr): Expr = {
    val egraph = EGraph.empty()
    val id = egraph.addExpr(BENF(e)._1)
    Runner.init().run(egraph, NoPredicate(), Seq(), CNF.directedRules, Seq(id))
    // val extractor = Extractor.init(egraph, LexicographicCost(AppCount, AstSize))
    // val (_, normalized) = extractor.findBestOf(id)
    val analyser = Analyser.init(egraph,
      AvoidCompositionAssoc1Extract(LexicographicCost(AppCount, AstSize)))
    val (avoidCount, _, normalized) = analyser.analysisOf(id).best
    assert(avoidCount == 0)
    ExprWithHashCons.expr(egraph)(normalized)
  }
}
