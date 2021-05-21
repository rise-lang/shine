package rise.eqsat

/** A class of equivalent nodes */
class EClass[D](val id: EClassId,
                val t: TypeId, // NOTE: this is close to analysis data
                var nodes: Vec[ENode],
                var data: D,
                var parents: Vec[(ENode, EClassId)]) {
  def nodeCount(): Int = nodes.size
}

// TODO: can we prove that infinite loops are not possible, is the procedure even correct?
// TODO: should the visited map have a more global scope for better memoization?
// FIXME: broken by hash-consing
object EClass {
  // TODO: generalize eclass transformation traversal?
  def shifted(eclass: EClassId, shift: Expr.Shift, cutoff: Expr.Shift, egraph: DefaultAnalysis.EGraph): EClassId = {
    // (eclass, shift, cutoff) -> result
    val visited = HashMap[(EClassId, Expr.Shift, Expr.Shift), EClassId]()

    def rec(_id: EClassId, shift: Expr.Shift, cutoff: Expr.Shift): EClassId = {
      val id = egraph.find(_id)
      visited.get(id, shift, cutoff) match {
        case Some(computedId) =>
          computedId
        case None =>
          val eclass = egraph.get(id)
          def shiftIsIdentity(maxIndex: Option[Int], shift: Int, cutoff: Int): Boolean =
            shift == 0 || maxIndex.forall(_ < cutoff)
          val nothingToDo =
            // TODO: could store max in analysis for speed?
            shiftIsIdentity(eclass.data.free.maxOption, shift._1, cutoff._1) &&
            shiftIsIdentity(eclass.data.freeNat.maxOption, shift._2, cutoff._2) &&
            shiftIsIdentity(eclass.data.freeDataType.maxOption, shift._3, cutoff._3)
          if (nothingToDo) {
            visited += (id, shift, cutoff) -> id
            id
          } else {
            val t = ???
              // eclass.t.shifted((shift._2, shift._3), (cutoff._2, cutoff._3)))
            val dummy = egraph.makeEmptyEClass(t)
            visited += (id, shift, cutoff) -> dummy
            val finalId = eclass.nodes.foldLeft(dummy) { case (currentId, enode) =>
              // visited += (egraph.find(id), shift, cutoff) -> currentId

              val newId = ??? /// egraph.add(NodeSubs.shifted(enode, shift, cutoff)(rec), t)
              egraph.union(newId, currentId)._1
            }
            visited += (egraph.find(id), shift, cutoff) -> finalId
            finalId
          }
      }
    }

    rec(eclass, shift, cutoff)
  }

  def replace(eclass: EClassId, index: Int, subs: EClassId, egraph: DefaultAnalysis.EGraph): EClassId = {
    // (eclass, index, subs) -> result
    val visited = HashMap[(EClassId, Int, EClassId), EClassId]()

    def rec(_id: EClassId, index: Int, subs: EClassId): EClassId = {
      val id = egraph.find(_id)
      visited.get(id, index, subs) match {
        case Some(computedId) =>
          computedId
        case None =>
          val eclass = egraph.get(id)
          val nothingToDo = !eclass.data.free.contains(index)
          if (nothingToDo) {
            visited += (id, index, subs) -> id
            id
          } else {
            val dummy = egraph.makeEmptyEClass(eclass.t)
            visited += (id, index, subs) -> dummy
            val finalId = eclass.nodes.foldLeft(dummy) { case (currentId, enode) =>
              // visited += (egraph.find(id), index, subs) -> currentId
              val newId = NodeSubs.replace(enode, index, subs) { n => egraph.add(n, eclass.t) }(rec) { case (e, s, c) => shifted(e, s, c, egraph) }
              // FIXME: bug
              if (egraph.get(newId).t != eclass.t) {
                println(s"${egraph.get(newId).t} != ${eclass.t}")
              }
              egraph.union(newId, currentId)._1
            }
            // FIXME: bug
            // TypeCheck(egraph)
            visited += (egraph.find(id), index, subs) -> finalId
            finalId
          }
      }
    }

    rec(eclass, index, subs)
  }

  def replace(eclass: EClassId, index: Int, subs: NatId, egraph: DefaultAnalysis.EGraph): EClassId = {
    // (eclass, index, subs) -> result
    val visited = HashMap[(EClassId, Int, NatId), EClassId]()

    def rec(_id: EClassId, index: Int, subs: NatId): EClassId = {
      val id = egraph.find(_id)
      visited.get(id, index, subs) match {
        case Some(computedId) =>
          computedId
        case None =>
          val eclass = egraph.get(id)
          if (!eclass.data.freeNat.contains(index)) {
            // nothing to do
            visited += (id, index, subs) -> id
            id
          } else {
            val t = ???
            // Type.simplifyNats(eclass.t.replace(index, subs))
            val dummy = egraph.makeEmptyEClass(t)
            visited += (id, index, subs) -> dummy
            val finalId = eclass.nodes.foldLeft(dummy) { case (currentId, enode) =>
              // visited += (egraph.find(id), index, subs) -> currentId
              val newId = ???
              // egraph.add(NodeSubs.replace(enode, index, subs)(rec), t)
              egraph.union(newId, currentId)._1
            }
            visited += (egraph.find(id), index, subs) -> finalId
            finalId
          }
      }
    }

    rec(eclass, index, subs)
  }

  // substitutes %0 for arg in body
  def withArgument(body: EClassId, arg: EClassId, egraph: DefaultAnalysis.EGraph): EClassId = {
    val sArg = shifted(arg, (1, 0, 0), (0, 0, 0), egraph)
    val rBody = replace(body, 0, sArg, egraph)
    shifted(rBody, (-1, 0, 0), (0, 0, 0), egraph)
  }

  // substitutes %n0 for arg in this
  def withNatArgument(body: EClassId, arg: NatId, egraph: DefaultAnalysis.EGraph): EClassId = {
    val sArg = NodeSubs.Nat.shifted(egraph, arg, 1, 0)
    val rBody = replace(body, 0, sArg, egraph)
    shifted(rBody, (0, -1, 0), (0, 0, 0), egraph)
  }
}