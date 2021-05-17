package rise.eqsat

/** A class of equivalent nodes */
class EClass[D](val id: EClassId,
                val t: Type, // NOTE: this is close to analysis data
                var nodes: Vec[ENode],
                var data: D,
                var parents: Vec[(ENode, EClassId)]) {
  def nodeCount(): Int = nodes.size
}
