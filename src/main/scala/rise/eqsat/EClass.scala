package rise.eqsat

/** A class of equivalent nodes */
class EClass(val id: EClassId,
             val t: TypeId, // NOTE: this is close to analysis data
             var nodes: Vec[ENode],
             var parents: Vec[(ENode, EClassId)]) {
  def nodeCount(): Int = nodes.size
}