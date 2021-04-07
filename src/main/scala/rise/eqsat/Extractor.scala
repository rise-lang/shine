package rise.eqsat

object Extractor {
  def init[C](egraph: EGraph[_], costFunction: CostFunction[C])
             (implicit costCmp: math.Ordering[C]): Extractor[C] = {
    val costs = HashMap.empty[EClassId, (C, ENode)]
    val e = new Extractor(costFunction, costs, egraph)
    e.computeCosts()
    e
  }
}

/** Extracts a single expression from an [[EGraph]],
  * which minimizes the given [[CostFunction]].
  */
class Extractor[Cost](val costFunction: CostFunction[Cost],
                      val costs: HashMap[EClassId, (Cost, ENode)],
                      val egraph: EGraph[_])
                     (implicit costCmp: math.Ordering[Cost]) {
  def findBestOf(eclass: EClassId): (Cost, Expr) =
    findBestRec(eclass, HashMap.empty)

  def findBestCostOf(eclass: EClassId): Cost =
    costs(egraph.find(eclass))._1

  private def findBestRec(eclass: EClassId,
                          addedMemo: HashMap[EClassId, Expr]
                         ): (Cost, Expr) = {
    val id = egraph.find(eclass)
    val (bestCost, bestNode) = costs(id)
    (bestCost, addedMemo.get(id) match {
      case Some(idExpr) => idExpr
      case None =>
        def childF(id: EClassId): Expr =
          findBestRec(id, addedMemo)._2
        val expr = Expr(bestNode.mapChildren(childF))
        assert(!addedMemo.contains(id))
        addedMemo += id -> expr
        expr
    })
  }

  private def nodeTotalCost(node: ENode): Option[Cost] = {
    val hasCost = node.children().forall(id => costs.contains(egraph.find(id)))
    if (hasCost) {
      val costF = (id: EClassId) => costs(egraph.find(id))._1
      Some(costFunction.cost(node, costF))
    } else {
      None
    }
  }

  private def computeCosts(): Unit = {
    var didSomething = true
    while (didSomething) {
      didSomething = false

      for (eclass <- egraph.classes.values) {
        (costs.get(eclass.id), makePass(eclass)) match {
          case (None, Some(newCost)) =>
            costs += eclass.id -> newCost
            didSomething = true
          case (Some(oldCost), Some(newCost)) if costCmp.lt(newCost._1, oldCost._1) =>
            costs += eclass.id -> newCost
            didSomething = true
          case _ => ()
        }
      }
    }

    for (eclass <- egraph.classes.values) {
      assert(costs.contains(eclass.id))
    }
  }

  private def makePass(eclass: EClass[_]): Option[(Cost, ENode)] = {
    val (cost, node) = eclass.nodes
      .map(n => (nodeTotalCost(n), n))
      .minBy(x => x._1)(MaybeCostOrdering)
    cost.map(c => (c, node))
  }

  object MaybeCostOrdering extends math.Ordering[Option[Cost]] {
    override def compare(a: Option[Cost], b: Option[Cost]): Int = {
      (a, b) match {
        case (None, None) => 0
        case (None, Some(_)) => 1
        case (Some(_), None) => -1
        case (Some(a), Some(b)) => costCmp.compare(a, b)
      }
    }
  }
}

/** A cost function which can be used by an [[Extractor]] */
trait CostFunction[Cost] {
  def cost(enode: ENode, costs: EClassId => Cost): Cost
}

object AstSize extends CostFunction[Int] {
  def cost(enode: ENode, costs: EClassId => Int): Int =
    enode.children().foldLeft(1) { case (acc, eclass) => acc + costs(eclass) }
}

object AstDepth extends CostFunction[Int] {
  def cost(enode: ENode, costs: EClassId => Int): Int =
    1 + enode.children().foldLeft(0) { case (acc, eclass) => acc.max(costs(eclass)) }
}