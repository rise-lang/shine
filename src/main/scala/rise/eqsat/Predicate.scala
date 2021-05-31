package rise.eqsat

import rise.eqsat.ematching.MNode

// TODO: think about what the predicate interface should be
trait Predicate[ED, ND, TD] {
  def apply(egraph: EGraph[ED, ND, TD], ec: EClass[ED]): Boolean
}

case class NoPredicate[ED, ND, TD]() extends Predicate[ED, ND, TD] {
  override def apply(egraph: EGraph[ED, ND, TD], ec: EClass[ED]): Boolean = true
}

case class ArrayDimensionPredicate[ED, ND, TD](limit: Int) extends Predicate[ED, ND, TD] {
  override def apply(egraph: EGraph[ED, ND, TD], ec: EClass[ED]): Boolean = {
    // TODO: e-class analysis?
    def countArrayDims(t: TypeId): Int = {
      egraph(t)._1 match {
        case FunType(inT, outT) =>
          countArrayDims(inT) max countArrayDims(outT)
        case NatFunType(t) => countArrayDims(t)
        case DataFunType(t) => countArrayDims(t)
        case ArrayType(_, et) => 1 + countArrayDims(et)
        case PairType(dt1, dt2) =>
          countArrayDims(dt1) max countArrayDims(dt2)
        case DataTypeVar(_) | ScalarType(_) | NatType |
             VectorType(_, _) | IndexType(_) => 0
      }
    }

    countArrayDims(ec.t) <= limit
  }
}

object Prototype {
  object Cost {
    val zero: Cost = Cost(0, HashMap.empty)

    def ofNode(n: ENode): Cost =
      Cost(1, HashMap(n.map(_ => (), _ => (), _ => ()) -> 1))
  }

  case class Cost(size: Int,
                  nodes: HashMap[MNode, Int]) {
    def +(other: Cost): Cost = {
      val psk = this.nodes.keySet ++ other.nodes.keySet
      val psm = psk
        .map { p => p -> (this.nodes(p) + other.nodes(p)) }
        .to(HashMap)
      Cost(this.size + other.size, psm)
    }

    def merge(other: Cost): Cost = {
      val psk = this.nodes.keySet intersect other.nodes.keySet
      val psm = psk
        .map { p => p -> (this.nodes(p) min other.nodes(p)) }
        .to(HashMap)
      Cost(this.size min other.size, psm)
    }
  }

  def countLimits(
    egraph: DefaultAnalysis.EGraph,
    roots: Seq[EClassId],
    // limit: Cost
  ): Unit = {
    import Node.eclassIdOrdering

    val limit = 50 // cut at 50 nodes long

    util.printTime("count limits", {
    // 1. Find the shortest path to the roots
    val costSoFar = HashMap(roots.map(r => r -> 0): _*) // cost zero
    val todo = scala.collection.mutable.TreeSet(roots: _*)

    while (todo.nonEmpty) {
      val id = todo.head
      todo.remove(id)

      val eclass = egraph.get(id)

      for (n <- eclass.nodes) {
        val costToChild = costSoFar(id) + 1 // + cost of node
        for (child <- n.children()) {
          val csf = costSoFar.get(child)
          csf.map(_ => costToChild)
          if (costSoFar.get(child).forall(costToChild < _)) { // cost merge
            costSoFar(child) = costToChild
            todo += child
          }
        }
      }
    }

    // 2. Count nodes which only participate in paths with too much cost
    //    (costSoFar + minimumCost)
    val count = costSoFar.count {
      case (id, c) =>
        val minimumCost = egraph.get(id).data.extracted.map(_._2.toDouble)
          .getOrElse(Double.PositiveInfinity)
        (c + minimumCost) > limit.toDouble
    }

    println(s"$count nodes could be pruned by size limit of $limit")
    })
  }
}
