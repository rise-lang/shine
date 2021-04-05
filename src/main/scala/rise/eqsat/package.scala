package rise

import scala.collection.mutable

package object eqsat {
  trait NodeTypes {
    type E // Expr
    type T // Type
    type DT // DataType
    type N // Nat
  }
  object EChildren extends NodeTypes {
    type E = EClassId
    type T = Type
    type DT = DataType
    type N = Nat
  }
  object ExprChildren extends NodeTypes {
    type E = Expr
    type T = Type
    type DT = DataType
    type N = Nat
  }
  object NoChildren extends NodeTypes {
    type E = ()
    type T = ()
    type DT = ()
    type N = ()
  }

  type TypePattern = Type
  type DataTypePattern = DataType
  type PNode = Node[Pattern, NatPattern, DataTypePattern]

  type ENode = Node[EClassId, Nat, DataType]

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
    runner.run(Seq(rules.eta, rules.beta))
    val extractor = Extractor.init(runner.egraph, AstSize)
    val (_, normalized) = extractor.findBestOf(id)
    normalized
  }
}
