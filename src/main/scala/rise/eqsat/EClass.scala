package rise.eqsat

/** A class of equivalent nodes */
class EClass[D](var id: EClassId,
                var nodes: Vec[ENode],
                var data: D,
                var parents: Vec[(ENode, EClassId)]) {
  def nodeCount(): Int = nodes.size
}
