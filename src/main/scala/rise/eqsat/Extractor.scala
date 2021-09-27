package rise.eqsat

object Extractor {
  def findBestOf[C](egraph: EGraph, costFunction: CostFunction[C], id: EClassId): (ExprWithHashCons, C) = {
    val analysis = SmallestCostAnalysis(costFunction)
    egraph.requireAnalysis(analysis)
    val result = egraph.getAnalysis(analysis)(id)
    egraph.releaseAnalysis(analysis)
    result
  }

  def randomOf(egraph: EGraph, id: EClassId): ExprWithHashCons = {
    val random = new scala.util.Random

    def rec(id: EClassId): ExprWithHashCons = {
      val eclass = egraph.get(id)
      val node = eclass.nodes(random.nextInt(eclass.nodes.length))
      ExprWithHashCons(node.mapChildren(rec), eclass.t)
    }

    rec(id)
  }

  def printRandom(egraph: EGraph, id: EClassId, n: Int): Unit = {
    for (_ <- 0 until n) {
      println(Expr.toNamed(ExprWithHashCons.expr(egraph)(randomOf(egraph, id))))
    }
  }
}

/*
/** Extracts a single expression from an [[EGraph]],
  * which minimizes the given [[CostFunction]].
  */
class Extractor[Cost](val costFunction: CostFunction[Cost],
                      val costs: HashMap[EClassId, (Cost, ENode)],
                      val egraph: EGraph) {
  def findBestOf(eclass: EClassId): (Cost, ExprWithHashCons) =
    findBestRec(eclass, HashMap.empty)

  def findBestCostOf(eclass: EClassId): Cost =
    costs(egraph.find(eclass))._1

  private def findBestRec(eclass: EClassId,
                          addedMemo: HashMap[EClassId, ExprWithHashCons]
                         ): (Cost, ExprWithHashCons) = {
    val id = egraph.find(eclass)
    val (bestCost, bestNode) = costs(id)
    (bestCost, addedMemo.get(id) match {
      case Some(idExpr) => idExpr
      case None =>
        def childF(id: EClassId): ExprWithHashCons =
          findBestRec(id, addedMemo)._2
        val expr = ExprWithHashCons(bestNode.mapChildren(childF), egraph.get(id).t)
        assert(!addedMemo.contains(id))
        addedMemo += id -> expr
        expr
    })
  }
}
*/

case class SmallestCostAnalysis[Cost](costFunction: CostFunction[Cost])
  extends AnalysisOps with SemiLatticeAnalysis
{
  // TODO: would (ENode, Int) be more efficient?
  type Data = (ExprWithHashCons, Cost)

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(), Set())

  override def make(egraph: EGraph, enode: ENode, t: TypeId): Data = {
    val smallestOf = egraph.getAnalysis(this)
    (ExprWithHashCons(enode.mapChildren(c => smallestOf(c)._1), t),
      costFunction.cost(enode, c => smallestOf(c)._2))
  }

  override def merge(a: Data, b: Data): MergeResult = {
    (a, b) match {
      case ((_, aCost), (_, bCost)) =>
        if (costFunction.ordering.gt(aCost, bCost)) {
          MergeResult(b, mayNotBeA = true, mayNotBeB = false)
        } else {
          MergeResult(a, mayNotBeA = false, mayNotBeB = true)
        }
    }
  }
}

/** A cost function which can be used by an [[Extractor]] */
trait CostFunction[Cost] {
  val ordering: math.Ordering[Cost]
  def cost(enode: ENode, costs: EClassId => Cost): Cost
}

object AstSize extends CostFunction[Int] {
  val ordering = implicitly
  def cost(enode: ENode, costs: EClassId => Int): Int =
    enode.children().foldLeft(1) { case (acc, eclass) => acc + costs(eclass) }
}

object AstDepth extends CostFunction[Int] {
  val ordering = implicitly
  def cost(enode: ENode, costs: EClassId => Int): Int =
    1 + enode.children().foldLeft(0) { case (acc, eclass) => acc.max(costs(eclass)) }
}

object AppCount extends CostFunction[Int] {
  val ordering = implicitly
  def cost(enode: ENode, costs: EClassId => Int): Int = {
    val thisCount = enode match {
      case App(_, _) => 1
      case _ => 0
    }
    enode.children().foldLeft(thisCount) { case (acc, eclass) => acc + costs(eclass) }
  }
}

case class LexicographicCost[A, B](a: CostFunction[A], b: CostFunction[B])
  extends CostFunction[(A, B)] {
  val ordering = {
    implicit val aOrd = a.ordering
    implicit val bOrd = b.ordering
    implicitly
  }
  override def cost(enode: ENode, costs: EClassId => (A, B)): (A, B) =
    (a.cost(enode, id => costs(id)._1), b.cost(enode, id => costs(id)._2))
}