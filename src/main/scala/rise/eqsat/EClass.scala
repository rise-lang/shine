package rise.eqsat

/** A class of equivalent nodes */
class EClass[D](val id: EClassId,
                val t: Type, // NOTE: this is close to analysis data
                var nodes: Vec[ENode],
                var data: D,
                var parents: Vec[(ENode, EClassId)]) {
  def nodeCount(): Int = nodes.size
}

object EClass {
  def replace[D](eclass: EClassId, index: Int, subs: EClassId, egraph: EGraph[D]): EClassId = {
    val visited = HashMap[EClassId, EClassId]()
    ???
  }

  def shifted[D](eclass: EClassId, shift: Expr.Shift, cutoff: Expr.Shift, egraph: EGraph[D]): EClassId = {
    ???
  }

  // substitutes %0 for arg in body
  def withArgument[D](body: EClassId, arg: EClassId, egraph: EGraph[D]): EClassId = {
    val tmp = replace(body, 0, shifted(arg, (1, 0, 0), (0, 0, 0), egraph), egraph)
    shifted(tmp, (-1, 0, 0), (0, 0, 0), egraph)
  }
}