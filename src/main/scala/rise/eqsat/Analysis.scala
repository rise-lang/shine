package rise.eqsat

sealed trait Order
case object Less extends Order
case object Equal extends Order
case object Greater extends Order

/** Explains how arbitrary analysis data associated with an [[EClass]]
  * is maintained across [[EGraph]] operations.
  * @see [[https://docs.rs/egg/0.6.0/egg/trait.Analysis.html]]
  */
trait Analysis[Data] {
  // useful if empty e-classes are created
  def empty(egraph: EGraph[Data], t: Type): Data

  def make(egraph: EGraph[Data], enode: ENode, t: Type): Data

  // - if `to < from` then `to` should be assigned to `from`
  // - if `to > from` then `to` should be unmodified
  // - if `to = from` then `to` should be unmodified
  // - if they cannot be compared, then `to` should be modified
  def merge(to: Data, from: Data): Option[Order]

  def modify(egraph: EGraph[Data], id: EClassId): Unit = {}

  def preUnion(egraph: EGraph[Data], id1: EClassId, id2: EClassId): Unit = {}
}

object NoAnalysis extends Analysis[()] {
  override def empty(egraph: EGraph[Unit], t: Type): Unit = ()
  override def make(egraph: EGraph[()], enode: ENode, t: Type): () = ()
  override def merge(to: (), from: ()): Option[Order] = Some(Equal)
}

class DefaultAnalysisData(var free: HashSet[Int],
                          var freeNat: HashSet[Int],
                          var freeDataType: HashSet[Int],
                          var extracted: Option[(Expr, Int)]) {
  def extractedExpr: Expr = extracted.get._1
  def extractedSize: Int = extracted.get._2
}

abstract class DefaultAnalysisCustomisable() extends Analysis[DefaultAnalysisData] {
  override def empty(egraph: EGraph[DefaultAnalysisData], t: Type): DefaultAnalysisData =
    new DefaultAnalysisData(HashSet(), HashSet(), HashSet(), None)

  def freeMerge(to: HashSet[Int], from: HashSet[Int]): Unit

  override def make(egraph: EGraph[DefaultAnalysisData], enode: ENode, t: Type): DefaultAnalysisData = {
    val free = HashSet.empty[Int]
    val freeNat = HashSet.empty[Int]
    val freeDataType = HashSet.empty[Int]

    def flatten(i: Seq[(Seq[Int], Seq[Int])]): (Seq[Int], Seq[Int]) =
      i.foldRight((Seq.empty[Int], Seq.empty[Int]))
      { case ((ns1, dts1), (ns2, dts2)) => (ns1 ++ ns2, dts1 ++ dts2) }

    def commit(ndts: (Seq[Int], Seq[Int])): Unit = {
      freeNat ++= ndts._1
      freeDataType ++= ndts._2
    }

    def collectFreeNat(n: Nat): (Seq[Int], Seq[Int]) = {
      n.node match {
        case NatVar(index) => (Seq(index), Seq())
        case node => flatten(node.map(collectFreeNat).nats().toSeq)
      }
    }

    def collectFreeDataType(dt: DataType): (Seq[Int], Seq[Int]) = {
      dt.node match {
        case DataTypeVar(index) => (Seq(), Seq(index))
        case node => flatten(DataTypeNode.collect(node.map(collectFreeNat, collectFreeDataType)))
      }
    }

    def collectFreeType(t: Type): (Seq[Int], Seq[Int]) = {
      t.node match {
        case FunType(inT, outT) =>
          flatten(Seq(collectFreeType(inT), collectFreeType(outT)))
        case NatFunType(t) =>
          val (ns, dts) = collectFreeType(t)
          (ns.filter(idx => idx != 0).map(idx => idx - 1), dts)
        case DataFunType(t) =>
          val (ns, dts) = collectFreeType(t)
          (ns, dts.filter(idx => idx != 0).map(idx => idx - 1))
        case dt: DataTypeNode[Nat, DataType] => collectFreeDataType(DataType(dt))
      }
    }

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
      case _ => enode.map({ c =>
        val cd = egraph.getMut(c).data
        free ++= cd.free
        freeNat ++= cd.freeNat
        freeDataType ++= cd.freeDataType
      }, { n => commit(collectFreeNat(n)) }, { dt => commit(collectFreeDataType(dt)) })
    }
    commit(collectFreeType(t))

    def computeExtracted(): Option[(Expr, Int)] = {
      var extractedSize = 1
      val extractedExpr = Expr(enode.mapChildren { c =>
        egraph.getMut(c).data.extracted match {
          case None => return None
          case Some((e, s)) =>
            extractedSize += s
            e
        }
      }, t)
      Some(extractedExpr, extractedSize)
    }

    new DefaultAnalysisData(free, freeNat, freeDataType, computeExtracted())
  }

  override def merge(to: DefaultAnalysisData, from: DefaultAnalysisData): Option[Order] = {
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

object DefaultAnalysis extends DefaultAnalysisCustomisable {
  override def freeMerge(to: HashSet[Int], from: HashSet[Int]): Unit =
    to ++= from // union
}

object DefaultAnalysisWithFreeIntersection extends DefaultAnalysisCustomisable {
  override def freeMerge(to: HashSet[Int], from: HashSet[Int]): Unit =
    to.filterInPlace(from.contains) // intersection
}