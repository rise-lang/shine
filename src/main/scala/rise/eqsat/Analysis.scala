package rise.eqsat

/** Explains how arbitrary analysis data associated with an [[EClass]]
  * is maintained across [[EGraph]] operations.
  * @see [[https://docs.rs/egg/0.6.0/egg/trait.Analysis.html]]
  */
trait Analysis {
  type Data

  def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis])

  def init(egraph: EGraph): ()
  def update(egraph: EGraph): ()

  // FIXME: we are currently not updating the analysis data to account for removals
  def eliminate(egraph: EGraph, toEliminate: HashSet[EClassId]): Unit = {
    val dataMap = egraph.getAnalysisMap(this)
    toEliminate.foreach { id => dataMap.remove(id) }
  }
}

trait TypeAnalysis {
  type Analyser
  type NatData
  type TypeData

  def requiredTypeAnalyses(): Set[TypeAnalysis]

  def makeNat(egraph: EGraph, node: NatNode[NatId]): NatData
  def makeType(egraph: EGraph,
               node: TypeNode[TypeId, NatId, DataTypeId]): TypeData
}

trait SemiLatticeAnalysis extends Analysis {
  def make(egraph: EGraph, enode: ENode, t: TypeId): Data

  // This should return the merge analysis data, which can be an updated `a`.
  //
  // The result is a `MergeResult(result, mayNotBeA, mayNotBeB)` indicating whether
  // the merged result may be different from `a` and `b` respectively,
  // where `a` denotes `a` before it might have been mutated.
  def merge(a: Data, b: Data): MergeResult

  // mayNotBe == !mustBe
  case class MergeResult(result: Data, mayNotBeA: Boolean, mayNotBeB: Boolean)

  override def init(egraph: EGraph): Unit = {
    assert(egraph.clean)
    val dataMap = egraph.getAnalysisMap(this)

    val analysisPending = HashSetQueuePop.empty[(ENode, EClassId)]

    egraph.classes.values.foreach { eclass =>
      eclass.nodes.foreach { enode =>
        if (enode.childrenCount() == 0) {
          analysisPending += ((enode, eclass.id))
        }
      }
    }

    resolvePendingAnalysis(egraph, this)(dataMap, analysisPending)

    assert {
      egraph.classes.keys.forall(dataMap.contains)
    }
  }

  override def update(egraph: EGraph): Unit = {
    val dataMap0 = egraph.getAnalysisMap(this)
    val dataMap = dataMap0.asInstanceOf[HashMap[EClassId, Data]] // FIXME: why?

    val analysisPending = HashSetQueuePop.empty[(ENode, EClassId)]

    egraph.analysisPending.foreach {
      case PendingMakeAnalysis(enode, id, t) =>
        dataMap += id -> this.make(egraph, enode, t)
      case PendingMergeAnalysis(a, aParents, b, bParents) =>
        val result = this.merge(dataMap(a), dataMap(b))
        if (result.mayNotBeA) {
          analysisPending ++= aParents
        }
        if (result.mayNotBeB) {
          analysisPending ++= bParents
        }
        dataMap += a -> result.result
        dataMap.remove(b)
    }

    resolvePendingAnalysis(egraph, this)(dataMap0, analysisPending)
  }

  private def resolvePendingAnalysis(egraph: EGraph,
                                     analysis: SemiLatticeAnalysis)(
                                     dataMap: HashMap[EClassId, analysis.Data],
                                     analysisPending: HashSetQueuePop[(ENode, EClassId)]): Unit = {
    while (analysisPending.nonEmpty) {
      val (node, id) = analysisPending.pop()
      val uNode = node.mapChildren(egraph.findMut)

      if (uNode.children().forall(dataMap.contains)) {
        val cid = egraph.findMut(id)
        val eclass = egraph.classes(cid)
        val node_data = analysis.make(egraph, uNode, eclass.t)
        val newData = dataMap.get(cid) match {
          case None =>
            analysisPending ++= eclass.parents
            node_data
          case Some(existing) =>
            val result = analysis.merge(existing, node_data)
            if (result.mayNotBeA) {
              analysisPending ++= eclass.parents
            }
            result.result
        }
        dataMap += cid -> newData
      } else {
        analysisPending += (node, id)
      }
    }
  }
}

trait CommutativeSemigroupAnalysis extends Analysis {
  def make(egraph: EGraph, enode: ENode, t: TypeId): Data

  def merge(a: Data, b: Data): Data

  override def init(egraph: EGraph): Unit = {
    assert(egraph.clean)
    val dataMap = egraph.getAnalysisMap(this)

    val analysisPending = HashSetQueuePop.empty[EClassId]

    egraph.classes.values.foreach { eclass =>
      eclass.nodes.foreach { enode =>
        if (enode.childrenCount() == 0) {
          analysisPending += eclass.id
        }
      }
    }

    resolvePendingAnalysis(egraph, this)(dataMap, analysisPending)

    assert {
      egraph.classes.keys.forall(dataMap.contains)
    }
  }

  override def update(egraph: EGraph): Unit = {
    val dataMap0 = egraph.getAnalysisMap(this)
    val dataMap = dataMap0.asInstanceOf[HashMap[EClassId, Data]] // FIXME: why?

    val analysisPending = HashSetQueuePop.empty[EClassId]

    egraph.analysisPending.foreach {
      case PendingMakeAnalysis(_, id, _) =>
        analysisPending += egraph.findMut(id)
      case PendingMergeAnalysis(a, _, b, _) =>
        analysisPending += egraph.findMut(a)
        dataMap.remove(b)
    }

    resolvePendingAnalysis(egraph, this)(dataMap0, analysisPending)

    assert {
      egraph.classes.keys.forall(dataMap.contains)
    }
  }

  private def resolvePendingAnalysis(egraph: EGraph,
                                     analysis: CommutativeSemigroupAnalysis)(
                                      dataMap: HashMap[EClassId, analysis.Data],
                                      analysisPending: HashSetQueuePop[EClassId]): Unit = {
    while (analysisPending.nonEmpty) {
      val id = analysisPending.pop()
      val cid = egraph.findMut(id)
      assert(cid == id)

      val eclass = egraph.classes(cid)
      val availableData = eclass.nodes.flatMap { n =>
        val uNode = n.mapChildren(egraph.findMut)
        // assert(n == uNode)

        if (uNode.children().forall(dataMap.contains)) {
          Some(analysis.make(egraph, uNode, eclass.t))
        } else {
          None
        }
      }
      if (availableData.isEmpty) {
        assert(eclass.nodes.nonEmpty)
        analysisPending += cid
      } else {
        val existingData = dataMap.get(eclass.id)
        val computedData = availableData.reduce[analysis.Data] { case (a, b) => analysis.merge(a, b) }
        if (!existingData.contains(computedData)) {
          dataMap += cid -> computedData
          analysisPending ++= eclass.parents.map(p => egraph.findMut(p._2))
        }
      }
    }
  }
}

object Analysis {
  def mergeRequired(a: (Set[Analysis], Set[TypeAnalysis]),
                    b: (Set[Analysis], Set[TypeAnalysis])): (Set[Analysis], Set[TypeAnalysis]) = {
    (a._1 ++ b._1, a._2 ++ b._2)
  }
}

object TypeAnalysis {
  def update(egraph: EGraph, a: TypeAnalysis): Unit = {
    val (natMap, typeMap) = egraph.getTypeAnalysisMaps(a)
    updateGeneric(egraph.hashConses.nats, natMap.keys.toSet) { case (id, n) =>
      val hasData = n.nats().forall(natMap.contains)
      if (hasData) { natMap += id -> a.makeNat(egraph, n) }
      hasData
    }
    updateGeneric(egraph.hashConses.dataTypes, typeMap.keys.toSet) { case (id, n) =>
      val hasData = DataTypeNode.collect(
        n.map(natMap.contains, typeMap.contains)).forall(x => x)
      if (hasData) { typeMap += id -> a.makeType(egraph, n) }
      hasData
    }
    updateGeneric(egraph.hashConses.types, typeMap.keys.toSet) { case (id, n) =>
      val hasData = TypeNode.collect(
        n.map(typeMap.contains, natMap.contains, typeMap.contains)).forall(x => x)
      if (hasData) { typeMap += id -> a.makeType(egraph, n) }
      hasData
    }
  }

  private def updateGeneric[Node, Id1 <: Id2, Id2](h: HashCons[Node, Id1],
                                                   alreadyAnalysed: Set[Id2])(
    tryAnalysis: (Id2, Node) => Boolean
  ): Unit = {
    var remaining = h.nodes.keySet.filterNot(alreadyAnalysed)
    while (remaining.nonEmpty) {
      remaining = remaining.filterNot { id =>
        tryAnalysis(id, h.get(id))
      }
    }
  }
}

object NoAnalysis extends SemiLatticeAnalysis with TypeAnalysis {
  type Data = ()
  type NatData = ()
  type TypeData = ()

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) = (Set(), Set())
  override def requiredTypeAnalyses(): Set[TypeAnalysis] = Set()

  override def make(egraph: EGraph, enode: ENode, t: TypeId): () = ()

  override def makeNat(egraph: EGraph, node: NatNode[NatId]): () = ()
  override def makeType(egraph: EGraph,
                        node: TypeNode[TypeId, NatId, DataTypeId]): () = ()

  override def merge(a: (), b: ()): MergeResult =
    MergeResult((), mayNotBeA = false, mayNotBeB = false)
}

// TODO: use SmallestCost Analysis
object SmallestSizeAnalysis extends SemiLatticeAnalysis {
  type Data = (ExprWithHashCons, Int)

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(), Set())

  override def make(egraph: EGraph, enode: ENode, t: TypeId): (ExprWithHashCons, Int) = {
    val smallestOf = egraph.getAnalysis(this)
    var size = 1
    val expr = ExprWithHashCons(enode.mapChildren { c =>
      val (e, s) = smallestOf(c)
      size += s
      e
    }, t)
    (expr, size)
  }

  override def merge(a: Data, b: Data): MergeResult = {
    (a, b) match {
      case ((_, aSize), (_, bSize)) =>
        assert(aSize > 0 && bSize > 0)
        if (aSize > bSize) {
          MergeResult(b, mayNotBeA = true, mayNotBeB = false)
        } else {
          MergeResult(a, mayNotBeA = false, mayNotBeB = true)
        }
    }
  }
}

class FreeData(var free: HashSet[Int],
               var freeNat: HashSet[Int],
               var freeDataType: HashSet[Int],
               var freeAddr: HashSet[Int])
class FreeNatData(var freeNat: HashSet[Int])
class FreeTypeData(var freeNat: HashSet[Int],
                   var freeDataType: HashSet[Int])

object FreeAnalysis extends FreeAnalysisCustomisable() {
  override def freeMerge(to: HashSet[Int], from: HashSet[Int]): Unit =
    to ++= from // union
}

object FreeIntersectionAnalysis extends FreeAnalysisCustomisable() {
  override def freeMerge(to: HashSet[Int], from: HashSet[Int]): Unit =
    to.filterInPlace(from.contains) // intersection
}

abstract class FreeAnalysisCustomisable() extends SemiLatticeAnalysis with TypeAnalysis {
  type Data = FreeData
  type NatData = FreeNatData
  type TypeData = FreeTypeData

  def freeMerge(to: HashSet[Int], from: HashSet[Int]): Unit

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) = (Set(), Set(this))
  override def requiredTypeAnalyses(): Set[TypeAnalysis] = Set()

  override def make(egraph: EGraph, enode: ENode, t: TypeId): Data = {
    val free = HashSet.empty[Int]
    val freeNat = HashSet.empty[Int]
    val freeDataType = HashSet.empty[Int]
    val freeAddr = HashSet.empty[Int]

    val freeOf = egraph.getAnalysis(this)
    val (freeOfNat, freeOfType) = egraph.getTypeAnalysis(this)

    enode match {
      case Var(index) =>
        free += index
      case Lambda(e) =>
        val ed = freeOf(e)
        free ++= ed.free.filter(idx => idx != 0).map(idx => idx - 1)
        freeNat ++= ed.freeNat
        freeDataType ++= ed.freeDataType
        freeAddr ++= ed.freeAddr
      case NatLambda(e) =>
        val ed = freeOf(e)
        free ++= ed.free
        freeNat ++= ed.freeNat.filter(idx => idx != 0).map(idx => idx - 1)
        freeDataType ++= ed.freeDataType
        freeAddr ++= ed.freeAddr
      case DataLambda(e) =>
        val ed = freeOf(e)
        free ++= ed.free
        freeNat ++= ed.freeNat
        freeDataType ++= ed.freeDataType.filter(idx => idx != 0).map(idx => idx - 1)
        freeAddr ++= ed.freeAddr
      case AddrLambda(e) =>
        val ed = freeOf(e)
        free ++= ed.free
        freeNat ++= ed.freeNat
        freeDataType ++= ed.freeDataType
        freeAddr ++= ed.freeAddr
      case _ => enode.map(
        { c =>
          val d = freeOf(c)
          free ++= d.free
          freeNat ++= d.freeNat
          freeDataType ++= d.freeDataType
          freeAddr ++= d.freeAddr
        },
        { n =>
          val d = freeOfNat(n)
          freeNat ++= d.freeNat
        },
        { dt =>
          val d = freeOfType(dt)
          freeNat ++= d.freeNat
          freeDataType ++= d.freeDataType
        },
        {
          case AddressVar(index) =>
            freeAddr += index
          case _ =>
        }
      )
    }
    {
      val d = freeOfType(t)
      freeNat ++= d.freeNat
      freeDataType ++= d.freeDataType
    }

    new Data(free, freeNat, freeDataType, freeAddr)
  }

  override def merge(a: Data, b: Data): MergeResult = {
    val beforeFreeCount = a.free.size
    val beforeFreeNatCount = a.freeNat.size
    val beforeFreeDataTypeCount = a.freeDataType.size
    val beforeFreeAddrCount = a.freeAddr.size
    freeMerge(a.free, b.free)
    freeMerge(a.freeNat, b.freeNat)
    freeMerge(a.freeDataType, b.freeDataType)
    freeMerge(a.freeAddr, b.freeAddr)

    MergeResult(a,
      mayNotBeA = beforeFreeCount != a.free.size ||
        beforeFreeNatCount != a.freeNat.size ||
        beforeFreeDataTypeCount != a.freeDataType.size ||
        beforeFreeAddrCount != a.freeAddr.size,
      mayNotBeB = beforeFreeCount != b.free.size ||
        beforeFreeNatCount != b.freeNat.size ||
        beforeFreeDataTypeCount != b.freeDataType.size ||
        beforeFreeAddrCount != b.freeAddr.size
    )
  }

  override def makeNat(egraph: EGraph, node: NatNode[NatId]): NatData = {
    val freeNat = HashSet[Int]()
    val (freeOfNat, _) = egraph.getTypeAnalysis(this)

    node match {
      case NatVar(index) => freeNat += index
      case node => node.map(n => freeNat ++= freeOfNat(n).freeNat)
    }
    new NatData(freeNat)
  }

  override def makeType(egraph: EGraph, node: TypeNode[TypeId, NatId, DataTypeId]): TypeData = {
    val freeNat = HashSet[Int]()
    val freeDataType = HashSet[Int]()

    val (freeOfNat, freeOfType) = egraph.getTypeAnalysis(this)

    def acc(d: TypeData): Unit = {
      freeNat ++= d.freeNat
      freeDataType ++= d.freeDataType
    }
    node match {
      case FunType(inT, outT) =>
        acc(freeOfType(inT))
        acc(freeOfType(outT))
      case NatFunType(t) =>
        val d = freeOfType(t)
        freeNat ++= d.freeNat.iterator.filter(idx => idx != 0).map(idx => idx - 1)
        freeDataType ++= d.freeDataType
      case DataFunType(t) =>
        val d = freeOfType(t)
        freeNat ++= d.freeNat
        freeDataType ++= d.freeDataType.iterator.filter(idx => idx != 0).map(idx => idx - 1)
      case AddrFunType(t) =>
        val d = freeOfType(t)
        freeNat ++= d.freeNat
        freeDataType ++= d.freeDataType
      case dt: DataTypeNode[NatId, DataTypeId] =>
        dt match {
          case DataTypeVar(index) => freeDataType += index
          case node => node.map(
            { n => freeNat ++= freeOfNat(n).freeNat },
            { dt => acc(freeOfType(dt)) })
        }
    }
    new TypeData(freeNat, freeDataType)
  }
}

object Beam {
  def merge[C, E](n: Int, cf: CostFunction[C], a: Seq[(C, E)], b: Seq[(C, E)]): Seq[(C, E)] = {
    // TODO: merge sort?
    // TODO: hash-cons the exprs for faster .distinct?
    (a ++ b).sortBy(_._1)(cf.ordering).distinct.take(n)
  }
}

// TODO: is this actually a Semi Lattice Analysis?
case class BeamExtract2[Cost](beamSize: Int, cf: CostFunction[Cost])
  extends CommutativeSemigroupAnalysis
{
  type Data = Seq[(Cost, ExprWithHashCons)]

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(), Set())

  override def make(egraph: EGraph, enode: ENode, t: TypeId): Seq[(Cost, ExprWithHashCons)] = {
    val analysisOf = egraph.getAnalysis(this)
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

  override def merge(a: Seq[(Cost, ExprWithHashCons)], b: Seq[(Cost, ExprWithHashCons)]): Seq[(Cost, ExprWithHashCons)] =
    Beam.merge(beamSize, cf, a, b)
}