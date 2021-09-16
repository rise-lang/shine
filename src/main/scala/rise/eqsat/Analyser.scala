package rise.eqsat

object Analyser {
  // FIXME: this is highly similar to the other Analysis trait and CostFunction trait
  // TODO: think about a proper interface, maybe there are more optimal analyses algorithms
  //       depending on the analysis properties.
  trait Analysis[Data] {
    def make(enode: ENode, t: TypeId, analysisOf: EClassId => Data): Data

    // `a` and `b` can be mutated and returned
    def merge(a: Data, b: Data): Data

    def update(existing: Data, computed: Data): Data
  }

  def init[D](egraph: EGraph, costFunction: CostFunction[D])
  (implicit costCmp: math.Ordering[D]): Analyser[D] = {
    init(egraph, new Analysis[D] {
      override def make(enode: ENode, t: TypeId, analysisOf: EClassId => D): D =
        costFunction.cost(enode, analysisOf)

      override def merge(a: D, b: D): D =
        costCmp.min(a, b)

      override def update(existing: D, computed: D): D = {
        assert(costCmp.gteq(existing, computed))
        computed
      }
    })
  }

  def init[D](egraph: EGraph, analysis: Analysis[D],
              data: HashMap[EClassId, D] = HashMap.empty[EClassId, D]): Analyser[D] = {
    val e = new Analyser(analysis, data, egraph)
    e.run()
    e
  }
}

class Analyser[Data](val analysis: Analyser.Analysis[Data],
                     val data: HashMap[EClassId, Data],
                     val egraph: EGraph) {
  def analysisOf(eclass: EClassId): Data =
    data(egraph.find(eclass))

  private def run(): Unit = {
    var didSomething = true

    // TODO: keep track of pending analysis for performance
    while (didSomething) {
      didSomething = false

      for (eclass <- egraph.classes.values) {
        didSomething = makePass(eclass) || didSomething
      }
    }

    for (eclass <- egraph.classes.values) {
      assert(data.contains(eclass.id))
    }
  }

  private def makePass(eclass: EClass): Boolean = {
    val existingData = data.get(eclass.id)
    val computedData = eclass.nodes.flatMap(n => analyseNode(n, eclass.t))
      .reduceOption(analysis.merge)
    (existingData, computedData) match {
      case (None, Some(newData)) =>
        data += eclass.id -> newData
        true
      case (Some(oldData), Some(newData)) =>
        val updated = analysis.update(oldData, newData)
        val changed = oldData != updated
        if (changed) {
          data += eclass.id -> newData
        }
        changed
      case (_, None) =>
        false
    }
  }


  private def analyseNode(node: ENode, t: TypeId): Option[Data] = {
    val hasData = node.children().forall(id => data.contains(egraph.find(id)))
    if (hasData) {
      Some(analysis.make(node, t, analysisOf))
    } else {
      None
    }
  }
}

case class CountProgramsUpToSize(limit: Int) extends Analyser.Analysis[HashMap[Int, Long]] {
  override def make(enode: ENode, t: TypeId,
                    analysisOf: EClassId => HashMap[Int, Long]): HashMap[Int, Long] = {
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

  override def update(existing: HashMap[Int, Long], computed: HashMap[Int, Long]): HashMap[Int, Long] =
    computed
}

case class BeamExtract[Cost](beamSize: Int, cf: CostFunction[Cost])
                            (implicit costCmp: math.Ordering[Cost])
  extends Analyser.Analysis[Seq[(Cost, ExprWithHashCons)]]
{
  override def make(enode: ENode, t: TypeId,
                    analysisOf: EClassId => Seq[(Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] = {
    val childrenBeams = enode.children().map(c => (c, analysisOf(c))).toSeq

    def rec(remaining: Seq[(EClassId, Seq[(Cost, ExprWithHashCons)])],
            selected: Map[EClassId, (Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] = {
      remaining match {
        case Nil =>
          Seq((
            cf.cost(enode, c => selected(c)._1),
            ExprWithHashCons(enode.mapChildren(c => selected(c)._2), t)))
        case (child, childBeam) +: rest =>
          childBeam.flatMap { x =>
            rec(rest, selected + (child -> x))
          }
      }
    }

    val tmp = rec(childrenBeams, Map.empty).sortBy(_._1).take(beamSize)
    assert(tmp == tmp.distinct)
    tmp
  }

  override def merge(a: Seq[(Cost, ExprWithHashCons)], b: Seq[(Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] = {
    val sorted = (a ++ b).sortBy(_._1)
    assert(sorted.size == sorted.distinct.size)
    sorted.take(beamSize)
  }

  override def update(existing: Seq[(Cost, ExprWithHashCons)], computed: Seq[(Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] = {
    val sorted = (existing ++ computed).sortBy(_._1)
    // TODO: hash-cons the exprs for faster .distinct?
    val dedup = sorted.distinct
      // sorted.headOption ++ sorted.iterator.sliding(2, 1).withPartial(false)
      // .flatMap(nbh => if (nbh(0) == nbh(1)) { None } else { Some(nbh(1)) })
    dedup.take(beamSize)
  }
}

object BeamExtract {
  def print[Cost](beamSize: Int, cf: CostFunction[Cost], egraph: EGraph, id: EClassId)
                 (implicit costCmp: math.Ordering[Cost]): Unit = {
    val analyser = Analyser.init(egraph, BeamExtract(beamSize, cf))
    analyser.analysisOf(id).foreach { case (cost, expr) =>
      println(s"Cost of $cost:")
      println(Expr.toNamed(ExprWithHashCons.expr(egraph)(expr)))
    }
  }
}