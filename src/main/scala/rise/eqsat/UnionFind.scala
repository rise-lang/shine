package rise.eqsat

import scala.annotation.tailrec

object UnionFind {
  def empty: UnionFind = new UnionFind(Vec[EClassId]())
}

final class UnionFind(var parents: Vec[EClassId]) {
  def makeSet(): EClassId = {
    val id = EClassId(parents.size)
    parents += id
    id
  }

  private def parent(of: EClassId): EClassId =
    parents(of.i)

  private def setParent(of: EClassId, p: EClassId): Unit =
    parents(of.i) = p

  def isRoot(id: EClassId): Boolean =
    parent(id) == id

  @tailrec
  def find(id: EClassId): EClassId = {
    val p = parent(id)
    if (id == p) { id } else { find(p) }
  }

  @tailrec
  def findMut(id: EClassId): EClassId = {
    val p = parent(id)
    val gp = parent(p)
    if (id == p) { id } else { setParent(id, gp); findMut(gp) }
  }

  def union(root1: EClassId, root2: EClassId): EClassId = {
    assert(isRoot(root1) && isRoot(root2) && root1 != root2)
    setParent(root2, root1)
    root1
  }
}
