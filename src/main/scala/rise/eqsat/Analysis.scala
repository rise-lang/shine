package rise.eqsat

/** Explains how arbitrary analysis data associated with an [[EClass]]
  * is maintained across [[EGraph]] operations.
  * @see [[https://docs.rs/egg/0.6.0/egg/trait.Analysis.html]]
  */
trait Analysis {
  type Data

  def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis])

  // returns true if newly computed data must be merged with previously computed data
  def accumulates(): Boolean

  // useful if empty e-classes are created
  def empty(egraph: EGraph, t: TypeId): Data

  def make(egraph: EGraph, enode: ENode, t: TypeId): Data

  // This should return the merge analysis data, which can be an updated `a`.
  //
  // The result is a `MergeResult(result, mayNotBeA, mayNotBeB)` indicating whether
  // the merged result may be different from `a` and `b` respectively,
  // where `a` denotes `a` before it might have been mutated.
  def merge(a: Data, b: Data): MergeResult

  // mayNotBe == !mustBe
  case class MergeResult(result: Data, mayNotBeA: Boolean, mayNotBeB: Boolean)

  // NOTE: removed for now
  // def modify(egraph: EGraph, id: EClassId): Unit = {}
  // def preUnion(egraph: EGraph, id1: EClassId, id2: EClassId): Unit = {}
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

object Analysis {
  def mergeRequired(a: (Set[Analysis], Set[TypeAnalysis]),
                    b: (Set[Analysis], Set[TypeAnalysis])): (Set[Analysis], Set[TypeAnalysis]) = {
    (a._1 ++ b._1, a._2 ++ b._2)
  }

  def init(egraph: EGraph, analysis: Analysis): Unit = {
    val dataOf = egraph.getAnalysis(analysis)

    val analysisPending = HashSetQueuePop.empty[(ENode, EClassId)]

    egraph.classes.values.foreach { eclass =>
      eclass.nodes.foreach { enode =>
        if (enode.childrenCount() == 0) {
          analysisPending += ((enode, eclass.id))
        }
      }
    }

    resolvePendingAnalysis(egraph, analysis)(dataOf, analysisPending)

    assert {
      egraph.classes.keys.forall(dataOf.contains)
    }
  }

  def update(egraph: EGraph, analysis: Analysis): Unit = {
    val dataOf0 = egraph.getAnalysis(analysis)
    val dataOf = dataOf0.asInstanceOf[HashMap[EClassId, analysis.Data]] // FIXME: why?

    val analysisPending = HashSetQueuePop.empty[(ENode, EClassId)]

    egraph.analysisPending.foreach {
      case PendingMakeAnalysis(enode, id, t) =>
        dataOf += id -> analysis.make(egraph, enode, t)
      case PendingMergeAnalysis(a, aParents, b, bParents) =>
        val result = analysis.merge(dataOf(a), dataOf(b))
        if (result.mayNotBeA) {
          analysisPending ++= aParents
        }
        if (result.mayNotBeB) {
          analysisPending ++= bParents
        }
        dataOf += a -> result.result
    }

    resolvePendingAnalysis(egraph, analysis)(dataOf0, analysisPending)
  }

  private def resolvePendingAnalysis(egraph: EGraph,
                                     analysis: Analysis)(
                                     dataOf: HashMap[EClassId, analysis.Data],
                                     analysisPending: HashSetQueuePop[(ENode, EClassId)]): Unit = {
    while (analysisPending.nonEmpty) {
      val (node, id) = analysisPending.pop()

      if (node.children().forall(dataOf.contains)) {
        val cid = egraph.findMut(id)
        val eclass = egraph.classes(cid)
        val node_data = analysis.make(egraph, node, eclass.t)
        val newData = dataOf.get(cid) match {
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
        dataOf += cid -> newData
      } else {
        analysisPending += (node, id)
      }
    }
  }
}

object TypeAnalysis {
  def update(egraph: EGraph, a: TypeAnalysis): Unit = {
    val (dataOfNat, dataOfType) = egraph.getTypeAnalysis(a)
    updateGeneric(egraph.hashConses.nats, dataOfNat.keys.toSet) { case (id, n) =>
      val hasData = n.nats().forall(dataOfNat.contains)
      if (hasData) { dataOfNat += id -> a.makeNat(egraph, n) }
      hasData
    }
    updateGeneric(egraph.hashConses.dataTypes, dataOfType.keys.toSet) { case (id, n) =>
      val hasData = DataTypeNode.collect(
        n.map(dataOfNat.contains, dataOfType.contains)).forall(x => x)
      if (hasData) { dataOfType += id -> a.makeType(egraph, n) }
      hasData
    }
    updateGeneric(egraph.hashConses.types, dataOfType.keys.toSet) { case (id, n) =>
      val hasData = TypeNode.collect(
        n.map(dataOfType.contains, dataOfNat.contains, dataOfType.contains)).forall(x => x)
      if (hasData) { dataOfType += id -> a.makeType(egraph, n) }
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

object NoAnalysis extends Analysis with TypeAnalysis {
  type Data = ()
  type NatData = ()
  type TypeData = ()

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) = (Set(), Set())
  override def requiredTypeAnalyses(): Set[TypeAnalysis] = Set()

  override def accumulates(): Boolean = false

  override def empty(egraph: EGraph, t: TypeId): () = ()
  override def make(egraph: EGraph, enode: ENode, t: TypeId): () = ()

  override def makeNat(egraph: EGraph, node: NatNode[NatId]): () = ()
  override def makeType(egraph: EGraph,
                        node: TypeNode[TypeId, NatId, DataTypeId]): () = ()

  override def merge(a: (), b: ()): MergeResult =
    MergeResult((), mayNotBeA = false, mayNotBeB = false)
}

object SmallestSizeAnalysis extends Analysis {
  type Data = Option[(ExprWithHashCons, Int)]

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(), Set())

  override def accumulates(): Boolean = false

  override def empty(egraph: EGraph, t: TypeId): Option[(ExprWithHashCons, Int)] = None

  override def make(egraph: EGraph, enode: ENode, t: TypeId): Option[(ExprWithHashCons, Int)] = {
    val smallestOf = egraph.getAnalysis(this)
    var size = 1
    val expr = ExprWithHashCons(enode.mapChildren { c =>
      smallestOf(c) match {
        case None => return None
        case Some((e, s)) =>
          size += s
          e
      }
    }, t)
    Some(expr, size)
  }

  override def merge(a: Data, b: Data): MergeResult = {
    (a, b) match {
      case (None, _) =>
        MergeResult(b, mayNotBeA = true, mayNotBeB = false)
      case (Some(_), None) =>
        MergeResult(a, mayNotBeA = false, mayNotBeB = true)
      case (Some((_, aSize)), Some((_, bSize))) =>
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
               var freeDataType: HashSet[Int])
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

abstract class FreeAnalysisCustomisable() extends Analysis with TypeAnalysis {
  type Data = FreeData
  type NatData = FreeNatData
  type TypeData = FreeTypeData

  def freeMerge(to: HashSet[Int], from: HashSet[Int]): Unit

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) = (Set(), Set(this))
  override def requiredTypeAnalyses(): Set[TypeAnalysis] = Set()

  override def accumulates(): Boolean = false

  override def empty(egraph: EGraph, t: TypeId): Data =
    new Data(HashSet(), HashSet(), HashSet())

  override def make(egraph: EGraph, enode: ENode, t: TypeId): Data = {
    val free = HashSet.empty[Int]
    val freeNat = HashSet.empty[Int]
    val freeDataType = HashSet.empty[Int]

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
      case NatLambda(e) =>
        val ed = freeOf(e)
        free ++= ed.free
        freeNat ++= ed.freeNat.filter(idx => idx != 0).map(idx => idx - 1)
        freeDataType ++= ed.freeDataType
      case DataLambda(e) =>
        val ed = freeOf(e)
        free ++= ed.free
        freeNat ++= ed.freeNat
        freeDataType ++= ed.freeDataType.filter(idx => idx != 0).map(idx => idx - 1)
      case _ => enode.map(
        { c =>
          val d = freeOf(c)
          free ++= d.free
          freeNat ++= d.freeNat
          freeDataType ++= d.freeDataType
        },
        { n =>
          val d = freeOfNat(n)
          freeNat ++= d.freeNat
        },
        { dt =>
          val d = freeOfType(dt)
          freeNat ++= d.freeNat
          freeDataType ++= d.freeDataType
        }
      )
    }
    {
      val d = freeOfType(t)
      freeNat ++= d.freeNat
      freeDataType ++= d.freeDataType
    }

    new Data(free, freeNat, freeDataType)
  }

  override def merge(a: Data, b: Data): MergeResult = {
    val beforeFreeCount = a.free.size
    val beforeFreeNatCount = a.freeNat.size
    val beforeFreeDataTypeCount = a.freeDataType.size
    freeMerge(a.free, b.free)
    freeMerge(a.freeNat, b.freeNat)
    freeMerge(a.freeDataType, b.freeDataType)

    MergeResult(a,
      mayNotBeA = beforeFreeCount != a.free.size ||
        beforeFreeNatCount != a.freeNat.size ||
        beforeFreeDataTypeCount != a.freeDataType.size,
      mayNotBeB = beforeFreeCount != b.free.size ||
        beforeFreeNatCount != b.freeNat.size ||
        beforeFreeDataTypeCount != b.freeDataType.size
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