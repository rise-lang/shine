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

  def init[D](egraph: EGraph, costFunction: CostFunction[D]): Analyser[D] = {
    init(egraph, new Analysis[D] {
      override def make(enode: ENode, t: TypeId, analysisOf: EClassId => D): D =
        costFunction.cost(enode, analysisOf)

      override def merge(a: D, b: D): D =
        costFunction.ordering.min(a, b)

      override def update(existing: D, computed: D): D = {
        assert(costFunction.ordering.gteq(existing, computed))
        computed
      }
    })
  }

  def init[D](egraph: EGraph, analysis: Analysis[D],
              data: HashMap[EClassId, D] = HashMap.empty[EClassId, D]): Analyser[D] = {
    assert(egraph.clean)
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
    val analysisPending = HashSetQueuePop.empty[EClassId]

    egraph.classes.values.foreach { eclass =>
      analysisPending += eclass.id
    }

    while (analysisPending.nonEmpty) {
      val id = analysisPending.pop()
      val cid = egraph.findMut(id)
      assert(cid == id)

      val eclass = egraph.classes(cid)
      val didSomething = makePass(eclass)
      if (didSomething) {
        analysisPending ++= eclass.parents.map(p => egraph.findMut(p._2))
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
          data += eclass.id -> updated
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

case class AvoidCompositionAssoc1ExtractData[Cost](
  best: (Int, Cost, ExprWithHashCons),
  bestNoComp: Option[(Int, Cost, ExprWithHashCons)])

case class AvoidCompositionAssoc1Extract[Cost](cf: CostFunction[Cost])
  extends Analyser.Analysis[AvoidCompositionAssoc1ExtractData[Cost]] {

  override def make(enode: ENode, t: TypeId,
                    analysisOf: EClassId => AvoidCompositionAssoc1ExtractData[Cost]
                   ): AvoidCompositionAssoc1ExtractData[Cost] = {
    val childrenAnalysis = enode.children().map(c => c -> analysisOf(c)).toMap
    enode match {
      case Composition(f, g) =>
        // g may be a Composition
        var avoidCount = 1 + childrenAnalysis.values.map(v => v.best._1).sum
        var cost = cf.cost(enode, c => childrenAnalysis(c).best._2)
        var expr = ExprWithHashCons(enode.mapChildren(c => childrenAnalysis(c).best._3), t)
        // g may not be a Composition
        childrenAnalysis(g).bestNoComp.foreach { gbnc =>
          val childrenAvoidCountNoComp = childrenAnalysis(f).best._1 + gbnc._1
          if (childrenAvoidCountNoComp < avoidCount) {
            avoidCount = childrenAvoidCountNoComp
            cost = cf.cost(enode, Map(
              f -> childrenAnalysis(f).best._2,
              g -> gbnc._2
            ))
            expr = ExprWithHashCons(enode.mapChildren(Map(
              f -> childrenAnalysis(f).best._3,
              g -> gbnc._3
            )), t)
          }
        }
        val compound = (avoidCount, cost, expr)
        AvoidCompositionAssoc1ExtractData(compound, None)
      case _ =>
        val avoidCount = childrenAnalysis.values.map(v => v.best._1).sum
        val cost = cf.cost(enode, c => childrenAnalysis(c).best._2)
        val expr = ExprWithHashCons(enode.mapChildren(c => childrenAnalysis(c).best._3), t)
        val compound = (avoidCount, cost, expr)
        AvoidCompositionAssoc1ExtractData(compound, Some(compound))
    }
  }

  override def merge(a: AvoidCompositionAssoc1ExtractData[Cost],
                     b: AvoidCompositionAssoc1ExtractData[Cost]): AvoidCompositionAssoc1ExtractData[Cost] = {
    implicit val costCmp: Ordering[Cost] = cf.ordering
    AvoidCompositionAssoc1ExtractData(
      Seq(a.best, b.best).minBy { case (avoidCount, cost, _) => (avoidCount, cost) },
      (a.bestNoComp ++ b.bestNoComp).minByOption { case (avoidCount, cost, _) => (avoidCount, cost) },
    )
  }

  override def update(existing: AvoidCompositionAssoc1ExtractData[Cost],
                      computed: AvoidCompositionAssoc1ExtractData[Cost]): AvoidCompositionAssoc1ExtractData[Cost] =
    computed
}

case class BeamExtract[Cost](beamSize: Int, cf: CostFunction[Cost])
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

    val tmp = rec(childrenBeams, Map.empty).sortBy(_._1)(cf.ordering).take(beamSize)
    assert(tmp == tmp.distinct)
    tmp
  }

  override def merge(a: Seq[(Cost, ExprWithHashCons)], b: Seq[(Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] = {
    val sorted = (a ++ b).sortBy(_._1)(cf.ordering)
    assert(sorted.size == sorted.distinct.size)
    sorted.take(beamSize)
  }

  override def update(existing: Seq[(Cost, ExprWithHashCons)], computed: Seq[(Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] = {
    val sorted = (existing ++ computed).sortBy(_._1)(cf.ordering)
    // TODO: hash-cons the exprs for faster .distinct?
    val dedup = sorted.distinct
      // sorted.headOption ++ sorted.iterator.sliding(2, 1).withPartial(false)
      // .flatMap(nbh => if (nbh(0) == nbh(1)) { None } else { Some(nbh(1)) })
    dedup.take(beamSize)
  }
}

object BeamExtract {
  def print[Cost](beamSize: Int, cf: CostFunction[Cost], egraph: EGraph, id: EClassId): Unit = {
    val analyser = Analyser.init(egraph, BeamExtract(beamSize, cf))
    analyser.analysisOf(id).foreach { case (cost, expr) =>
      println(s"Cost of $cost:")
      println(Expr.toNamed(ExprWithHashCons.expr(egraph)(expr)))
    }
  }
}