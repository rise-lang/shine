package rise.eqsat

import rise.core.{types => rct}

/** Explains how arbitrary analysis data associated with an [[EClass]]
  * is maintained across [[EGraph]] operations.
  * @see [[https://docs.rs/egg/0.6.0/egg/trait.Analysis.html]]
  */
trait Analysis {
  type Data

  // FIXME: requiring non-type analysis is not supported
  def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis])

  def init(egraph: EGraph): Unit
  def update(egraph: EGraph): Unit

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

object SemiLatticeAnalysis {
  def oneShot(analysis: SemiLatticeAnalysis, egraph: EGraph)
             (dataMap: HashMap[EClassId, analysis.Data]) = {
    assert(egraph.clean)

    val analysisPending = HashSetQueuePop.empty[(ENode, EClassId)]

    // every node which has access to initial data needs to be analysed
    egraph.classes.values.foreach { eclass =>
      eclass.nodes.foreach { enode =>
        if (enode.children().forall(c => dataMap.contains(egraph.findMut(c)))) {
          analysisPending += ((enode, eclass.id))
        }
      }
    }

    resolvePendingAnalysis(egraph, analysis)(dataMap, analysisPending)

    assert {
      egraph.classes.keys.forall(dataMap.contains)
    }
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
        val node_data = analysis.make(egraph, uNode, eclass.t, dataMap)
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

trait SemiLatticeAnalysis extends Analysis {
  import SemiLatticeAnalysis.resolvePendingAnalysis

  // FIXME: what if we want to access other analyses? analysisOf would not be enough
  def make(egraph: EGraph, enode: ENode, t: TypeId,
           analysisOf: EClassId => Data): Data

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
        dataMap += id -> this.make(egraph, enode, t, dataMap)
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
}

trait CommutativeSemigroupAnalysis extends Analysis {
  def make(egraph: EGraph, enode: ENode, t: TypeId,
           analysisOf: EClassId => Data): Data

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
          Some(analysis.make(egraph, uNode, eclass.t, dataMap))
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
  // perform one-shot analysis without adding it to the on-the-fly e-graph analyses.
  def oneShot(analysis: Analysis, egraph: EGraph): HashMap[EClassId, analysis.Data] = {
    assert(egraph.clean)
    egraph.requireAnalysis(analysis)
    val result = egraph.getAnalysisMap(analysis)
    egraph.releaseAnalysis(analysis)
    result
  }

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
  type Data = Unit
  type NatData = Unit
  type TypeData = Unit

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) = (Set(), Set())
  override def requiredTypeAnalyses(): Set[TypeAnalysis] = Set()

  override def make(egraph: EGraph, enode: ENode, t: TypeId,
                    analysisOf: EClassId => Unit): Unit = ()

  override def makeNat(egraph: EGraph, node: NatNode[NatId]): Unit = ()
  override def makeType(egraph: EGraph,
                        node: TypeNode[TypeId, NatId, DataTypeId]): Unit = ()

  override def merge(a: Unit, b: Unit): MergeResult =
    MergeResult((), mayNotBeA = false, mayNotBeB = false)
}

// TODO: use SmallestCost Analysis
object SmallestSizeAnalysis extends SemiLatticeAnalysis {
  type Data = (ExprWithHashCons, Int)

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(), Set())

  override def make(egraph: EGraph, enode: ENode, t: TypeId,
                    smallestOf: EClassId => Data): Data = {
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

  override def make(egraph: EGraph, enode: ENode, t: TypeId,
                    freeOf: EClassId => Data): Data = {
    val free = HashSet.empty[Int]
    val freeNat = HashSet.empty[Int]
    val freeDataType = HashSet.empty[Int]
    val freeAddr = HashSet.empty[Int]

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

  def merge2[C, E](n: Int, cf: CostFunction[C], a: Seq[(C, E)], b: Seq[(C, E)]): (Seq[(C, E)], Boolean, Boolean) = {
    // TODO: more efficient
    val r = merge(n, cf, a, b)
    (r, r != a, r != b)
  }
}

/** An analysis to extract a beam of programs:
  * a sequence of at most `beamSize` programs minimizing a given cost
  * @todo figure out a way to increase diversity (e.g. extract modulo a given normal form) */
case class BeamExtract[Cost](beamSize: Int, cf: CostFunction[Cost])
  extends SemiLatticeAnalysis
{
  type Data = Seq[(Cost, ExprWithHashCons)]

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(), Set())

  override def make(egraph: EGraph, enode: ENode, t: TypeId,
                    analysisOf: EClassId => Data): Data = {
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

  override def merge(a: Data, b: Data): MergeResult = {
    val (r, mayNotBeA, mayNotBeB) = Beam.merge2(beamSize, cf, a, b)
    MergeResult(r, mayNotBeA, mayNotBeB)
  }
}

object BeamExtract {
  def print[Cost](beamSize: Int, cf: CostFunction[Cost], egraph: EGraph, id: EClassId): Unit = {
    val analysis = Analysis.oneShot(BeamExtract(beamSize, cf), egraph)
    analysis(egraph.find(id)).foreach { case (cost, expr) =>
      println(s"Cost of $cost:")
      println(Expr.toNamed(ExprWithHashCons.expr(egraph)(expr)))
    }
  }
}


object BeamExtractRW {
  sealed trait TypeAnnotation
  case class NotDataTypeAnnotation(node: TypeNode[TypeAnnotation, Unit, rct.Access])
    extends TypeAnnotation
  {
    override def toString: String = node.toString()
  }
  case class DataTypeAnnotation(access: rct.Access)
    extends TypeAnnotation
  {
    override def toString: String = access.toString()
  }

  type Data[Cost] = Map[(TypeAnnotation, Map[Int, TypeAnnotation]), Seq[(Cost, ExprWithHashCons)]]

  def merge[Cost](beamSize: Int, cf: CostFunction[Cost],
                  a: Data[Cost], b: Data[Cost]): Data[Cost] = {
    (a.keySet union b.keySet).map { ta =>
      ta -> ((a.get(ta), b.get(ta)) match {
        case (Some(x), Some(y)) => Beam.merge(beamSize, cf, x, y)
        case (None, Some(x)) => x
        case (Some(x), None) => x
        case (None, None) => throw new Exception("this should not happen")
      })
    }.toMap
  }

  def mergeEnv(a: Map[Int, TypeAnnotation], b: Map[Int, TypeAnnotation]): Option[Map[Int, TypeAnnotation]] = {
    def rec(keys: Seq[Int], acc: Map[Int, TypeAnnotation]): Option[Map[Int, TypeAnnotation]] = {
      keys match {
        case Nil => Some(acc)
        case i +: rest =>
          (a.get(i), b.get(i)) match {
            case (None, None) => throw new Exception("this should not happen")
            case (None, Some(x)) => rec(rest, acc + (i -> x))
            case (Some(x), None) => rec(rest, acc + (i -> x))
            case (Some(x), Some(y)) =>
              if (x == y) {
                rec(rest, acc + (i -> x))
              } else {
                None
              }
          }
      }
    }

    rec((a.keySet union b.keySet).toSeq, Map.empty)
  }

  def subtype(a: TypeAnnotation, at: TypeId, b: TypeAnnotation, bt: TypeId, egraph: EGraph): Boolean = {
    assert(at == bt)
    val res = (a, b) match {
      case (DataTypeAnnotation(x), DataTypeAnnotation(y)) =>
        (x == y) || (x == rct.read && notContainingArrayType(bt.asInstanceOf[DataTypeId], egraph))
      case (NotDataTypeAnnotation(x), NotDataTypeAnnotation(y)) =>
        (x, egraph(at), y, egraph(bt)) match {
          case (FunType(aIn, aOut), FunType(aInT, aOutT), FunType(bIn, bOut), FunType(bInT, bOutT)) =>
            subtype(bIn, bInT, aIn, aInT, egraph) && subtype(aOut, aOutT, bOut, bOutT, egraph)
          case (NatFunType(aOut), NatFunType(aOutT), NatFunType(bOut), NatFunType(bOutT)) =>
            subtype(aOut, aOutT, bOut, bOutT, egraph)
          case (DataFunType(aOut), DataFunType(aOutT), DataFunType(bOut), DataFunType(bOutT)) =>
            subtype(aOut, aOutT, bOut, bOutT, egraph)
          case _ => throw new Exception("this should not happen")
        }
      case _ => throw new Exception("this should not happen")
    }
    // println(s"subtype: $a : ${egraph(at)} <= $b : ${egraph(bt)} ? $res")
    res
  }

  // TODO: could hash-cons this
  def notContainingArrayType(t: DataTypeId, egraph: EGraph): Boolean = {
    egraph(t) match {
      case DataTypeVar(_) => false
      case ScalarType(_) | NatType | VectorType(_, _) |  IndexType(_) => true
      case PairType(dt1, dt2) => notContainingArrayType(dt1, egraph) && notContainingArrayType(dt2, egraph)
      case ArrayType(_, _) => false
    }
  }
}

object RWAnnotationDSL {
  import BeamExtractRW._
  val read = DataTypeAnnotation(rct.read)
  val write = DataTypeAnnotation(rct.write)

  implicit final class RWAnnotationOps(private val a: TypeAnnotation) extends AnyVal {
    @inline def ->:(b: TypeAnnotation): TypeAnnotation = NotDataTypeAnnotation(FunType(b, a))
  }

  def nFunT(a: TypeAnnotation): TypeAnnotation = NotDataTypeAnnotation(NatFunType(a))
  def dtFunT(a: TypeAnnotation): TypeAnnotation = NotDataTypeAnnotation(DataFunType(a))
  def aFunT(a: TypeAnnotation): TypeAnnotation = NotDataTypeAnnotation(AddrFunType(a))
}

/** An analysis to extract a beam of programs with valid DPIA read/write annotations.
  * @todo figure out a way to increase diversity (e.g. extract modulo a given normal form)
  * @todo could actually extract DPIA terms directly, by-passing `fromRise`?
  */
case class BeamExtractRW[Cost](beamSize: Int, cf: CostFunction[Cost])
  extends SemiLatticeAnalysis
{
  import BeamExtractRW.{mergeEnv, subtype, NotDataTypeAnnotation, TypeAnnotation}
  import RWAnnotationDSL._

  type Data = BeamExtractRW.Data[Cost]

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(), Set())

  override def make(egraph: EGraph, enode: ENode, t: TypeId,
                    analysisOf: EClassId => Data): Data = {
    // val childrenBeams = enode.children().map(c => (c, analysisOf(c))).toSeq
    // TODO: write more generic code here
    val generatedData: Data = enode match {
      case Var(index) =>
        val cost = cf.cost(egraph, enode, t, Map.empty)
        val expr = ExprWithHashCons(enode.mapChildren(Map.empty), t)
        Seq(read, write).map { annotation =>
          (annotation, Map(index -> annotation)) -> Seq((cost, expr))
        }.toMap
      case App(f, e) =>
        val fInT = egraph(egraph.get(f).t) match {
          case FunType(inT, _) => inT
          case _ => throw new Exception("app expected fun type")
        }
        val eT = egraph.get(e).t

        val fBeams = analysisOf(f)
        val eBeams = analysisOf(e)
        var newBeams: Data = Map.empty
        fBeams.foreach { case ((fAnnotation, fEnv), fBeam) =>
          fAnnotation match {
            case NotDataTypeAnnotation(FunType(fIn, fOut)) =>
              eBeams.foreach { case ((eAnnotation, eEnv), eBeam) =>
                mergeEnv(fEnv, eEnv).foreach { mergedEnv =>
                  if (subtype(eAnnotation, eT, fIn, fInT, egraph)) {
                    val newBeam = fBeam.flatMap { x => eBeam.flatMap { y =>
                      Seq((
                        cf.cost(egraph, enode, t, Map(f -> x._1, e -> y._1)),
                        ExprWithHashCons(enode.mapChildren(Map(f -> x._2, e -> y._2)), t)
                      ))
                    }}
                    newBeams = newBeams.updatedWith((fOut, mergedEnv)) {
                      case None => Some(newBeam)
                      case Some(prevBeam) => Some(prevBeam ++ newBeam)
                    }
                  }
                }
              }
            case _ => throw new Exception("app expected fun type")
          }
        }
        newBeams
      case Lambda(e) =>
        val eBeams = analysisOf(e)
        var newBeams: Data = Map.empty
        eBeams.foreach { case ((annotation, env), beam) =>
          val newEnv = env.filter(kv => kv._1 != 0).map(kv => (kv._1 - 1, kv._2))
          val newBeam = beam.flatMap { x =>
            Seq((
              cf.cost(egraph, enode, t, Map(e -> x._1)),
              ExprWithHashCons(enode.mapChildren(Map(e -> x._2)), t)
            ))
          }
          val identAnnotations = env.get(0).map(Seq(_)).getOrElse(Seq(read, write))
          identAnnotations.foreach { ina =>
            newBeams = newBeams.updatedWith((NotDataTypeAnnotation(FunType(ina, annotation)), newEnv)) {
              case None => Some(newBeam)
              case Some(prevBeam) => Some(prevBeam ++ newBeam)
            }
          }
        }
        newBeams
      case NatApp(f, _) =>
        val fBeams = analysisOf(f)
        fBeams.map { case ((annotation, env), beam) =>
          val newBeam = beam.flatMap { x =>
            Seq((
              cf.cost(egraph, enode, t, Map(f -> x._1)),
              ExprWithHashCons(enode.mapChildren(Map(f -> x._2)), t)
            ))
          }
          annotation match {
            case NotDataTypeAnnotation(NatFunType(at)) => (at, env) -> newBeam
            case _ => throw new Exception("natApp expected NatFunType")
          }
        }
      case DataApp(f, _) =>
        val fBeams = analysisOf(f)
        fBeams.map { case ((annotation, env), beam) =>
          val newBeam = beam.flatMap { x =>
            Seq((
              cf.cost(egraph, enode, t, Map(f -> x._1)),
              ExprWithHashCons(enode.mapChildren(Map(f -> x._2)), t)
            ))
          }
          annotation match {
            case NotDataTypeAnnotation(DataFunType(at)) => (at, env) -> newBeam
            case _ => throw new Exception("dataApp expected DataFunType")
          }
        }
      case AddrApp(f, _) =>
        val fBeams = analysisOf(f)
        fBeams.map { case ((annotation, env), beam) =>
          val newBeam = beam.flatMap { x =>
            Seq((
              cf.cost(egraph, enode, t, Map(f -> x._1)),
              ExprWithHashCons(enode.mapChildren(Map(f -> x._2)), t)
            ))
          }
          annotation match {
            case NotDataTypeAnnotation(AddrFunType(at)) => (at, env) -> newBeam
            case _ => throw new Exception("addrApp expected AddrFunType")
          }
        }
      case NatLambda(e) =>
        val eBeams = analysisOf(e)
        eBeams.map { case ((annotation, env), beam) =>
          val newBeam = beam.flatMap { x =>
            Seq((
              cf.cost(egraph, enode, t, Map(e -> x._1)),
              ExprWithHashCons(enode.mapChildren(Map(e -> x._2)), t)
            ))
          }
          // note: recording NatFunType() constructor is useless
          (NotDataTypeAnnotation(NatFunType(annotation)), env) -> newBeam
        }
      case DataLambda(e) =>
        val eBeams = analysisOf(e)
        eBeams.map { case ((annotation, env), beam) =>
          val newBeam = beam.flatMap { x =>
            Seq((
              cf.cost(egraph, enode, t, Map(e -> x._1)),
              ExprWithHashCons(enode.mapChildren(Map(e -> x._2)), t)
            ))
          }
          // note: recording DataFunType() constructor is useless
          (NotDataTypeAnnotation(DataFunType(annotation)), env) -> newBeam
        }
      case AddrLambda(e) =>
        val eBeams = analysisOf(e)
        eBeams.map { case ((annotation, env), beam) =>
          val newBeam = beam.flatMap { x =>
            Seq((
              cf.cost(egraph, enode, t, Map(e -> x._1)),
              ExprWithHashCons(enode.mapChildren(Map(e -> x._2)), t)
            ))
          }
          // note: recording DataFunType() constructor is useless
          (NotDataTypeAnnotation(AddrFunType(annotation)), env) -> newBeam
        }
      case Literal(_) | NatLiteral(_) | IndexLiteral(_, _) =>
        val beam = Seq((
          cf.cost(egraph, enode, t, Map.empty),
          ExprWithHashCons(enode.mapChildren(Map.empty), t)
        ))
        Map((read, Map.empty[Int, TypeAnnotation]) -> beam)
      case Primitive(p) =>
        import rise.core.{primitives => rp}
        import rise.openMP.{primitives => rompp}
        import rise.openCL.{primitives => roclp}
        import rise.Cuda.{primitives => rocup}

        val annotations = p match {
          case roclp.mapGlobal(_) | roclp.mapWorkGroup(_) | roclp.mapLocal(_)
               | rocup.mapGlobal(_) | rocup.mapBlock(_) | rocup.mapThreads(_)
               | rocup.mapWarp(_) | rocup.mapLane(_) | rompp.mapPar()
               | rp.mapSeq() | rp.mapSeqUnroll() | rp.iterateStream() => Seq(
            (read ->: write) ->: read ->: write
          )
          case rp.map() | rp.mapFst() | rp.mapSnd() => Seq(
            (read ->: read) ->: read ->: read,
            (write ->: write) ->: write ->: write,
          )
          case rp.mapStream() => Seq(
            (read ->: write) ->: read ->: read
          )
          case rp.toMem() => Seq(
            write ->: read
          )
          case roclp.oclRunPrimitive() => Seq(
            nFunT(nFunT(nFunT(nFunT(nFunT(nFunT(write ->: write))))))
          )
          case roclp.oclToMem() => Seq(
            aFunT(write ->: read)
          )
          case rp.join() | rp.transpose() | rp.asScalar() | rp.unzip() => Seq(
            read ->: read,
            write ->: write
          )
          case rp.vectorFromScalar() | rp.neg() | rp.not() | rp.indexAsNat() |
               rp.fst() | rp.snd()  | rp.cast() => Seq(
            read ->: read
          )
          case rp.let() => Seq(
            read ->: (read ->: read) ->: read,
            read ->: (read ->: write) ->: write,
          )
          case rp.split() | rp.asVector() => Seq(
            nFunT(read ->: read),
            nFunT(write ->: write),
          )
          case rp.asVectorAligned() => Seq(
            nFunT(read ->: read),
            // FIXME: DPIA accepts write -> write but OpenMP codegen fails
          )
          case rp.zip() | rp.makePair() => Seq(
            read ->: read ->: read,
            write ->: write ->: write,
          )
          case rp.idx() | rp.add() | rp.sub() | rp.mul() | rp.div() | rp.gt()
               | rp.lt() | rp.equal() | rp.mod() | rp.gather() => Seq(
            read ->: read ->: read
          )
          case rp.natAsIndex() | rp.take() | rp.drop() => Seq(
            nFunT(read ->: read)
          )
          case rp.reduceSeq() | rp.reduceSeqUnroll() => Seq(
            (read ->: read ->: write) ->: write ->: read ->: read
          )
          case roclp.oclReduceSeq() | roclp.oclReduceSeqUnroll() => Seq(
            aFunT((read ->: read ->: write) ->: write ->: read ->: read)
          )
          case rp.rotateValues() => Seq(
            nFunT((read ->: write) ->: read ->: read)
          )
          case rp.circularBuffer() => Seq(
            nFunT(nFunT((read ->: write) ->: read ->: read))
          )
          case roclp.oclRotateValues() => Seq(
            aFunT(nFunT((read ->: write) ->: read ->: read))
          )
          case roclp.oclCircularBuffer() => Seq(
            aFunT(nFunT(nFunT((read ->: write) ->: read ->: read)))
          )
          case rp.slide() | rp.padClamp() => Seq(
            nFunT(nFunT(read ->: read))
          )
          case rp.select() => Seq(
            read ->: read ->: read ->: read
          )
          case rp.padEmpty() => Seq(
            nFunT(write ->: write)
          )
          case rp.padCst() => Seq(
            nFunT(nFunT(read ->: read ->: read))
          )
          case rp.generate() => Seq(
            (read ->: read) ->: read
          )
          case rp.makeArray(n) =>
            def rec(n: Int): TypeAnnotation = {
              if (n > 0) {
                read ->: rec(n - 1)
              } else {
                read
              }
            }
            Seq(rec(n))
          case rp.id() =>
            // FIXME: only supports non-functional values
            Seq(read ->: read, write ->: write)
          case rp.foreignFunction(_, _) =>
            def buildAnnot(t: rise.eqsat.TypeId): TypeAnnotation = egraph(t) match {
              case _: DataTypeNode[NatId, DataTypeId] => read
              case rise.eqsat.FunType(in, out) => buildAnnot(in) ->: buildAnnot(out)
              case rise.eqsat.DataFunType(t) => dtFunT(buildAnnot(t))
              case node => throw new Exception(s"did not expect $node")
            }
            Seq(buildAnnot(t))
          case _ => throw new Exception(s"did not expect $p")
        }
        val beam = Seq((
          cf.cost(egraph, enode, t, Map.empty),
          ExprWithHashCons(enode.mapChildren(Map.empty), t)
        ))
        annotations.map { a => (a, Map.empty[Int, TypeAnnotation]) -> beam }.toMap
      case Composition(f, g) => ???
    }
    generatedData.map { case (at, beam) => at -> beam.sortBy(_._1)(cf.ordering).distinct.take(beamSize) }
  }

  override def merge(a: Data, b: Data): MergeResult = {
    val r = BeamExtractRW.merge(beamSize, cf, a, b)
    // FIXME: more efficient merge result?
    MergeResult(r, r != a, r != b)
  }
}

case class CountProgramsUpToSize(limit: Int) extends CommutativeSemigroupAnalysis {
  override type Data = HashMap[Int, Long]

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) = (Set(), Set())

  override def make(egraph: EGraph, enode: ENode, t: TypeId,
                    analysisOf: EClassId => Data): Data = {
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


case class AvoidCompositionAssoc1ExtractData[Cost](best: (Int, Cost, ExprWithHashCons),
                                                   bestNoComp: Option[(Int, Cost, ExprWithHashCons)])

/** An analysis to extract programs, minimizing right-associativity as well as a given cost */
case class AvoidCompositionAssoc1Extract[Cost](cf: CostFunction[Cost])
  extends SemiLatticeAnalysis
{
  type Data = AvoidCompositionAssoc1ExtractData[Cost]

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(), Set())

  override def make(egraph: EGraph, enode: ENode, t: TypeId,
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

  override def merge(a: Data, b: Data): MergeResult = {
    implicit val costCmp: Ordering[Cost] = cf.ordering
    val r = AvoidCompositionAssoc1ExtractData(
      Seq(a.best, b.best).minBy { case (avoidCount, cost, _) => (avoidCount, cost) },
      (a.bestNoComp ++ b.bestNoComp).minByOption { case (avoidCount, cost, _) => (avoidCount, cost) },
    )
    MergeResult(r,
      mayNotBeA = r.best != a.best || r.bestNoComp != a.bestNoComp,
      mayNotBeB = r.best != b.best || r.bestNoComp != b.bestNoComp)
  }
}