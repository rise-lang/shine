package rise

import scala.collection.mutable

/** Equality Saturation for Rise,
  * based on the [[https://egraphs-good.github.io/ `egg` library]].
  */
package object eqsat {
  type ENode = Node[EClassId, NatId, DataTypeId]
  type PNode = Node[Pattern, NatPattern, DataTypePattern]

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

  // TODO: could keep hash-consed nats/types?
  def BENF(e: Expr): Expr = {
    val egraph = EGraph.emptyWithAnalysis(DefaultAnalysis)
    val id = egraph.addExpr(e)
    Runner.init().run(egraph, NoPredicate(), Seq(
      rules.eta,//.directed(),
      // rules.beta, rules.betaNat
      rules.betaExtract,//.directed(),
      rules.betaNatExtract,//.directed()
    ), Seq(id))
    val extractor = Extractor.init(egraph, AstSize)
    val (_, normalized) = extractor.findBestOf(id)
    ExprWithHashCons.expr(egraph)(normalized)
  }

  // Combinator Normal Form
  def CNF(e: Expr): Expr = {
    val egraph = EGraph.emptyWithAnalysis(DefaultAnalysis)
    val id = egraph.addExpr(BENF(e))
    // FIXME: avoid using .directed()?
    Runner.init().run(egraph, NoPredicate(), Seq(
      rules.eta.directed(),
      // rules.beta, rules.betaNat,
      // rules.betaExtract.directed(),
      // rules.betaNatExtract.directed(),
      rules.combinatory.compositionIntro.directed(),
      // rules.combinatory.compositionAssoc1.directed()
      rules.combinatory.compositionAssoc2.directed(),
    ), Seq(id))
    val extractor = Extractor.init(egraph, LexicographicCost(AppCount, AstSize))
    val (_, normalized) = extractor.findBestOf(id)
    ExprWithHashCons.expr(egraph)(normalized)
  }
}
