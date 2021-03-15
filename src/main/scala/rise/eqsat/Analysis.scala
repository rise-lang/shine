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