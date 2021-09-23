package rise.eqsat

object Extractor {
  def init[C](egraph: EGraph, costFunction: CostFunction[C]): Extractor[C] = {
    val costs = HashMap.empty[EClassId, (C, ENode)]
    val e = new Extractor(costFunction, costs, egraph)
    e.computeCosts()
    e
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
          case (Some(oldCost), Some(newCost))
            if costFunction.ordering.lt(newCost._1, oldCost._1) =>
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

  private def makePass(eclass: EClass): Option[(Cost, ENode)] = {
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
        case (Some(a), Some(b)) =>
          costFunction.ordering.compare(a, b)
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