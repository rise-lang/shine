package rise.eqsat

import scala.collection.mutable.PriorityQueue

trait Predicate[ED, ND, TD] {
  def apply(egraph: EGraph[ED, ND, TD], id: EClassId): Boolean
}

case class NoPredicate[ED, ND, TD]() extends Predicate[ED, ND, TD] {
  def apply(egraph: EGraph[ED, ND, TD], id: EClassId): Boolean = true
}

case class AstSizePredicate(limit: Int) extends DefaultAnalysis.Predicate {
  def apply(egraph: DefaultAnalysis.EGraph, id: EClassId): Boolean =
    egraph.get(id).data.extracted.exists(_._2 <= limit)
}

object Dijkstra {
  private case class Todo(cost: Int, id: EClassId)

  private implicit val todoOrd: math.Ordering[Todo] = new Ordering[Todo] {
    override def compare(x: Todo, y: Todo): Int =
      x.cost compare y.cost
  }

  def countAstSizeLimit[ED, ND, TD](egraph: EGraph[ED, ND, TD],
                                    roots: Seq[EClassId],
                                    limit: Int): Int = {
    def nodeCost(n: ENode) = 1

    val costSoFar = HashMap(roots.map(r => r -> 0): _*)
    val todo = PriorityQueue(roots.map(r => Todo(0, r)): _*)

    while (todo.nonEmpty) {
      val Todo(_, id) = todo.dequeue()
      val eclass = egraph.get(id)

      for (n <- eclass.nodes) {
        val costToChild = costSoFar(id) + nodeCost(n)
        for (child <- n.children()) {
          if (costSoFar.get(child).forall(costToChild < _)) {
            costSoFar(child) = costToChild
            todo.addOne(Todo(costToChild, child))
          }
        }
      }
    }

    costSoFar.count { case (_, c) => c > limit }
  }
}