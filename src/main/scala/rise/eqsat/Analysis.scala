package rise.eqsat

sealed trait Order
case object Less extends Order
case object Equal extends Order
case object Greater extends Order

/** Explains how arbitrary analysis data associated with an [[EClass]]
  * is maintained across [[EGraph]] operations.
  * @see [[https://docs.rs/egg/0.6.0/egg/trait.Analysis.html]]
  * @todo try to reduce the generics boilerplate with Scala 3 features
  */
trait Analysis[ED, ND, TD] {
  // useful if empty e-classes are created
  def empty(egraph: EGraph[ED, ND, TD], t: TypeId): ED

  def make(egraph: EGraph[ED, ND, TD], enode: ENode, t: TypeId): ED
  def makeNat(hc: HashConses[ND, TD], node: NatNode[NatId]): ND
  def makeType(hc: HashConses[ND, TD],
               node: TypeNode[TypeId, NatId, DataTypeId]): TD

  // - if `to < from` then `to` should be assigned to `from`
  // - if `to > from` then `to` should be unmodified
  // - if `to = from` then `to` should be unmodified
  // - if they cannot be compared, then `to` should be modified
  def merge(to: ED, from: ED): Option[Order]

  def modify(egraph: EGraph[ED, ND, TD], id: EClassId): Unit = {}

  def preUnion(egraph: EGraph[ED, ND, TD], id1: EClassId, id2: EClassId): Unit = {}
}

object NoAnalysis extends Analysis[(), (), ()] {
  override def empty(egraph: EGraph[(), (), ()], t: TypeId): Unit = ()
  override def make(egraph: EGraph[(), (), ()], enode: ENode, t: TypeId): () = ()

  override def makeNat(hc: HashConses[(), ()], node: NatNode[NatId]): () = ()
  override def makeType(hc: HashConses[(), ()],
                        node: TypeNode[TypeId, NatId, DataTypeId]): () = ()

  override def merge(to: (), from: ()): Option[Order] = Some(Equal)
}

object DefaultAnalysis extends DefaultAnalysisCustomisable() {
  override def freeMerge(to: HashSet[Int], from: HashSet[Int]): Unit =
    to ++= from // union

  type Data = DefaultAnalysisCustomisable.Data
  type NatData = DefaultAnalysisCustomisable.NatData
  type TypeData = DefaultAnalysisCustomisable.TypeData

  type EGraph = rise.eqsat.EGraph[Data, NatData, TypeData]
  type HashConses = rise.eqsat.HashConses[NatData, TypeData]
  type Searcher = rise.eqsat.Searcher[Data, NatData, TypeData]
  type Applier = rise.eqsat.Applier[Data, NatData, TypeData]
  type Rewrite = rise.eqsat.Rewrite[Data, NatData, TypeData]
  type Predicate = rise.eqsat.Predicate[Data, NatData, TypeData]
}

object DefaultAnalysisCustomisable {
  class Data(var free: HashSet[Int],
             var freeNat: HashSet[Int],
             var freeDataType: HashSet[Int],
             var extracted: Option[(ExprWithHashCons, Int)]) {
    def extractedExpr: ExprWithHashCons = extracted.get._1
    def extractedSize: Int = extracted.get._2
  }

  class NatData(var freeNat: HashSet[Int])
  class TypeData(var freeNat: HashSet[Int],
                 var freeDataType: HashSet[Int])
}

abstract class DefaultAnalysisCustomisable() extends Analysis[
  DefaultAnalysisCustomisable.Data,
  DefaultAnalysisCustomisable.NatData,
  DefaultAnalysisCustomisable.TypeData
] {
  import DefaultAnalysis._

  override def empty(egraph: EGraph, t: TypeId): Data =
    new Data(HashSet(), HashSet(), HashSet(), None)

  def freeMerge(to: HashSet[Int], from: HashSet[Int]): Unit

  override def make(egraph: EGraph, enode: ENode, t: TypeId): Data = {
    val free = HashSet.empty[Int]
    val freeNat = HashSet.empty[Int]
    val freeDataType = HashSet.empty[Int]

    enode match {
      case Var(index) =>
        free += index
      case Lambda(e) =>
        val ed = egraph.getMut(e).data
        free ++= ed.free.filter(idx => idx != 0).map(idx => idx - 1)
        freeNat ++= ed.freeNat
        freeDataType ++= ed.freeDataType
      case NatLambda(e) =>
        val ed = egraph.getMut(e).data
        free ++= ed.free
        freeNat ++= ed.freeNat.filter(idx => idx != 0).map(idx => idx - 1)
        freeDataType ++= ed.freeDataType
      case DataLambda(e) =>
        val ed = egraph.getMut(e).data
        free ++= ed.free
        freeNat ++= ed.freeNat
        freeDataType ++= ed.freeDataType.filter(idx => idx != 0).map(idx => idx - 1)
      case _ => enode.map(
        { c =>
          val d = egraph.getMut(c).data
          free ++= d.free
          freeNat ++= d.freeNat
          freeDataType ++= d.freeDataType
        },
        { n =>
          val d = egraph(n)._2
          freeNat ++= d.freeNat
        },
        { dt =>
          val d = egraph(dt)._2
          freeNat ++= d.freeNat
          freeDataType ++= d.freeDataType
        }
      )
    }
    {
      val d = egraph(t)._2
      freeNat ++= d.freeNat
      freeDataType ++= d.freeDataType
    }

    def computeExtracted(): Option[(ExprWithHashCons, Int)] = {
      var extractedSize = 1
      val extractedExpr = ExprWithHashCons(enode.mapChildren { c =>
        egraph.getMut(c).data.extracted match {
          case None => return None
          case Some((e, s)) =>
            extractedSize += s
            e
        }
      }, t)
      Some(extractedExpr, extractedSize)
    }

    new Data(free, freeNat, freeDataType, computeExtracted())
  }

  override def makeNat(hc: HashConses, node: NatNode[NatId]): NatData = {
    val freeNat = HashSet[Int]()
    node match {
      case NatVar(index) => freeNat += index
      case node => node.map(n => freeNat ++= hc(n)._2.freeNat)
    }
    new NatData(freeNat)
  }

  override def makeType(hc: HashConses, node: TypeNode[TypeId, NatId, DataTypeId]): TypeData = {
    val freeNat = HashSet[Int]()
    val freeDataType = HashSet[Int]()
    def acc(d: TypeData): Unit = {
      freeNat ++= d.freeNat
      freeDataType ++= d.freeDataType
    }
    node match {
      case FunType(inT, outT) =>
        acc(hc(inT)._2)
        acc(hc(outT)._2)
      case NatFunType(t) =>
        val d = hc(t)._2
        freeNat ++= d.freeNat.iterator.filter(idx => idx != 0).map(idx => idx - 1)
        freeDataType ++= d.freeDataType
      case DataFunType(t) =>
        val d = hc(t)._2
        freeNat ++= d.freeNat
        freeDataType ++= d.freeDataType.iterator.filter(idx => idx != 0).map(idx => idx - 1)
      case dt: DataTypeNode[NatId, DataTypeId] =>
        dt match {
          case DataTypeVar(index) => freeDataType += index
          case node => node.map(
            { n => freeNat ++= hc(n)._2.freeNat },
            { dt => acc(hc(dt)._2) })
        }
    }
    new TypeData(freeNat, freeDataType)
  }

  override def merge(to: Data, from: Data): Option[Order] = {
    val beforeFreeCount = to.free.size
    val beforeFreeNatCount = to.freeNat.size
    val beforeFreeDataTypeCount = to.freeDataType.size
    freeMerge(to.free, from.free)
    freeMerge(to.freeNat, from.freeNat)
    freeMerge(to.freeDataType, from.freeDataType)
    var didChange =
      beforeFreeCount != to.free.size ||
        beforeFreeNatCount != to.freeNat.size ||
        beforeFreeDataTypeCount != to.freeDataType.size
    (to.extracted, from.extracted) match {
      case (None, _) =>
        to.extracted = from.extracted
        didChange = true
      case (Some(_), None) =>
      case (Some((_, toESize)), Some((_, fromESize))) =>
        assert(toESize > 0 && fromESize > 0)
        if (toESize > fromESize) {
          to.extracted = from.extracted
          didChange = true
        }
    }
    if (didChange) { None } else { Some(Greater) }
  }
}

object DefaultAnalysisWithFreeIntersection extends DefaultAnalysisCustomisable() {
  override def freeMerge(to: HashSet[Int], from: HashSet[Int]): Unit =
    to.filterInPlace(from.contains) // intersection
}