package rise.eqsat

sealed trait Order
case object Less extends Order
case object Equal extends Order
case object Greater extends Order

trait Analysis[Data] {
  def make(egraph: EGraph[Data], enode: ENode): Data

  // - if `to < from` then `to` should be assigned to `from`
  // - if `to > from` then `to` should be unmodified
  // - if `to = from` then `to` should be unmodified
  // - if they cannot be compared, then `to` should be modified
  def merge(to: Data, from: Data): Option[Order]

  def modify(egraph: EGraph[Data], id: EClassId): Unit = {}

  def preUnion(egraph: EGraph[Data], id1: EClassId, id2: EClassId): Unit = {}
}

object NoAnalysis extends Analysis[()] {
  override def make(egraph: EGraph[()], enode: ENode): () = ()
  override def merge(to: (), from: ()): Option[Order] = Some(Equal)
}

class DefaultAnalysisData(var free: HashSet[Int],
                          var extractedExpr: Expr,
                          var extractedSize: Int)

object DefaultAnalysis extends Analysis[DefaultAnalysisData] {
  override def make(egraph: EGraph[DefaultAnalysisData], enode: ENode): DefaultAnalysisData = {
    val free = HashSet.empty[Int]
    enode match {
      case Var(index) => free += index
      case Lambda(e) =>
        free ++= egraph(e).data.free.filter(idx => idx != 0).map(idx => idx - 1)
      case DepLambda(_, _) => ???
      case _ => enode.children().foreach(c => free ++= egraph(c).data.free)
    }
    val extractedExpr = Expr(enode.mapChildren(c => egraph(c).data.extractedExpr))
    val extractedSize = enode.children().foldLeft(1) { case (acc, c) => acc + egraph(c).data.extractedSize }
    new DefaultAnalysisData(free, extractedExpr, extractedSize)
  }

  override def merge(to: DefaultAnalysisData, from: DefaultAnalysisData): Option[Order] = {
    val beforeFreeCount = to.free.size
    to.free ++= from.free
    var didChange = beforeFreeCount != to.free.size
    if (to.extractedSize > from.extractedSize) {
      to.extractedExpr = from.extractedExpr
      to.extractedSize = from.extractedSize
      didChange = true
    }
    if (didChange) { None } else { Some(Greater) }
  }
}