package rise.eqsat

object Analyser {
  // FIXME: this is highly similar to the other Analysis trait and CostFunction trait
  trait Analysis[Data] {
    def make(enode: ENode, analysisOf: EClassId => Data): Data

    // `a` and `b` can be mutated and returned
    def merge(a: Data, b: Data): Data
  }

  def init[D](egraph: EGraph[_, _, _], costFunction: CostFunction[D])
  (implicit costCmp: math.Ordering[D]): Analyser[D] = {
    init(egraph, new Analysis[D] {
      override def make(enode: ENode, analysisOf: EClassId => D): D =
        costFunction.cost(enode, analysisOf)

      override def merge(a: D, b: D): D =
        costCmp.min(a, b)
    })
  }

  def init[D](egraph: EGraph[_, _, _], analysis: Analysis[D]): Analyser[D] = {
    val e = new Analyser(analysis, HashMap.empty[EClassId, D], egraph)
    e.run()
    e
  }
}

class Analyser[Data](val analysis: Analyser.Analysis[Data],
                     val data: HashMap[EClassId, Data],
                     val egraph: EGraph[_, _, _]) {
  def analysisOf(eclass: EClassId): Data =
    data(egraph.find(eclass))

  private def run(): Unit = {
    var didSomething = true

    while (didSomething) {
      didSomething = false

      for (eclass <- egraph.classes.values) {
        (data.get(eclass.id), makePass(eclass)) match {
          case (None, Some(newData)) =>
            data += eclass.id -> newData
            didSomething = true
          case (Some(oldData), Some(newData)) if oldData != newData =>
            data += eclass.id -> newData
            didSomething = true
          case _ => () // no new data, or new data does not change anything
        }
      }
    }

    for (eclass <- egraph.classes.values) {
      assert(data.contains(eclass.id))
    }
  }

  private def makePass(eclass: EClass[_]): Option[Data] =
    eclass.nodes.flatMap(analyseNode).reduceOption(analysis.merge)

  private def analyseNode(node: ENode): Option[Data] = {
    val hasData = node.children().forall(id => data.contains(egraph.find(id)))
    if (hasData) {
      Some(analysis.make(node, analysisOf))
    } else {
      None
    }
  }
}

case class CountProgramsUpToSize(limit: Int) extends Analyser.Analysis[HashMap[Int, Long]] {
  override def make(enode: ENode, analysisOf: EClassId => HashMap[Int, Long]): HashMap[Int, Long] = {
    val counts = HashMap.empty[Int, Long]
    val childrenCounts = enode.children().map(analysisOf).toSeq

    def rec(remaining: Seq[HashMap[Int, Long]], size: Int, count: Long): Unit = {
      if (size > limit) {
        return
      }
      remaining match {
        case Nil =>
          val total = counts.getOrElse(size, 0L) + count
          counts += (size -> total)
        case childCounts +: rest =>
          childCounts.foreach { case (s, c) =>
            rec(rest, size + s, count * c)
          }
      }
    }

    rec(childrenCounts, 1, 1)
    counts
  }

  override def merge(a: HashMap[Int, Long], b: HashMap[Int, Long]): HashMap[Int, Long] = {
    b.foreach { case (size, count) =>
      val total = a.getOrElse(size, 0L) + count
      a += size -> total
    }
    a
  }
}