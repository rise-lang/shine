package rise

import scala.collection.mutable

/** Equality Saturation for Rise,
  * based on the [[https://egraphs-good.github.io/ `egg` library]].
  */
package object eqsat {
  type ENode = Node[EClassId, Nat, DataType]
  type PNode = Node[Pattern, NatPattern, DataTypePattern]

  /** A key to identify [[EClass]]es within an [[EGraph]] */
  case class EClassId(i: Int)

  type Vec[T] = mutable.ArrayBuffer[T]
  val Vec: mutable.ArrayBuffer.type = mutable.ArrayBuffer
  type HashMap[K, V] = mutable.HashMap[K, V]
  val HashMap: mutable.HashMap.type = mutable.HashMap
  type HashSet[V] = mutable.HashSet[V]
  val HashSet: mutable.HashSet.type = mutable.HashSet

  def BENF(e: Expr): Expr = {
    val runner = Runner.withAnalysis(DefaultAnalysis)
    val id = runner.egraph.addExpr(e)
    runner.run(Seq(rules.eta, rules.beta, rules.betaNat))
    val extractor = Extractor.init(runner.egraph, AstSize)
    val (_, normalized) = extractor.findBestOf(id)
    normalized
  }
}
