package rise.eqsat

object EGraph {
  def emptyWithAnalysis[D](analysis: Analysis[D]): EGraph[D] = new EGraph(
    analysis = analysis,
    memo = HashMap.empty,
    classes = HashMap.empty[EClassId, EClass[D]],
    unionFind = UnionFind.empty,
    pending = Vec.empty,
    analysisPending = HashSet.empty,
    classesByMatch = HashMap.empty
  )
}

class EGraph[Data](
  val analysis: Analysis[Data],
  var pending: Vec[(ENode, EClassId)],
  var analysisPending: HashSet[(ENode, EClassId)],
  var memo: HashMap[ENode, EClassId],
  var unionFind: UnionFind,
  var classes: HashMap[EClassId, EClass[Data]],
  var classesByMatch: HashMap[Int, HashSet[EClassId]]
) {
  def nodeCount(): Int =
    classes.map { case (_, c) => c.nodeCount() }.iterator.sum
  def classCount(): Int =
    classes.size

  def find(id: EClassId): EClassId =
    unionFind.find(id)
  def findMut(id: EClassId): EClassId =
    unionFind.findMut(id)

  def get(id: EClassId): EClass[Data] =
    classes(find(id))
  def getMut(id: EClassId): EClass[Data] =
    classes(findMut(id))

  // returns the canonicalized enode and its eclass if it has one
  def lookup(enode: ENode): (ENode, Option[EClassId]) = {
    val enode2 = enode.mapChildren(find)
    val id = memo.get(enode2)
    (enode2, id.map(find))
  }

  def add(n: ENode): EClassId = {
    val (enode, optec) = lookup(n)
    optec.getOrElse {
      val id = unionFind.makeSet()
      val eclass = new EClass(
        id = id,
        nodes = Vec(enode),
        data = analysis.make(this, enode),
        parents = Vec())

      enode.children().foreach { c =>
        this.getMut(c).parents += enode -> id
      }

      pending += enode -> id // TODO: is this needed?
      classes += id -> eclass
      assert(!memo.contains(enode))
      memo += enode -> id

      analysis.modify(this, id)
      id
    }
  }

  def addExpr(expr: Expr): EClassId =
    add(expr.node.mapChildren(addExpr))

  // checks whether two expressions are equivalent
  // returns a list of eclasses that represent both expressions
  def equivs(e1: Expr, e2: Expr): Vec[EClassId] = {
    val matches1 = Pattern.fromExpr(e1).compile().search(this)
    val matches2 = Pattern.fromExpr(e2).compile().search(this)
    val equivClasses = Vec.empty[EClassId]
    for (m1 <- matches1) {
      val ec1 = find(m1.eclass)
      for (m2 <- matches2) {
        if (ec1 == find(m2.eclass)) {
          equivClasses += ec1
        }
      }
    }
    equivClasses
  }

  def dot(): EGraphDot = EGraphDot(this)

  // returns the merged eclass id and whether a union was done
  def union(id1: EClassId, id2: EClassId): (EClassId, Boolean) = {
    val cid1 = findMut(id1)
    val cid2 = findMut(id2)
    if (cid1 == cid2) { return (cid1, false) }
    unionCanonicalDiff(cid1, cid2)
  }

  private def unionCanonicalDiff(cid1: EClassId, cid2: EClassId): (EClassId, Boolean) = {
    // make sure class2 has fewer parents
    val parents1 = classes(cid1).parents.size
    val parents2 = classes(cid2).parents.size
    val (id1, id2) = if (parents1 < parents2) { (cid2, cid1) } else { (cid1, cid2) }
    assert(id1 != id2)

    // make id1 the new root
    analysis.preUnion(this, id1, id2)
    unionFind.union(id1, id2)
    val class2 = classes.remove(id2).get
    val class1 = classes(id1)
    assert(id1 == class1.id)

    pending ++= class2.parents
    analysis.merge(class1.data, class2.data) match {
      case Some(Equal) => ()
      case Some(Greater) => analysisPending ++= class2.parents
      case Some(Less) => analysisPending ++= class1.parents
      case None =>
        analysisPending ++= class1.parents
        analysisPending ++= class2.parents
    }

    class1.nodes ++= class2.nodes
    class1.parents ++= class2.parents

    analysis.modify(this, id1)
    (id1, true)
  }

  def rebuild(): Int = {
    val nUnions = processUnions()
    val _ = rebuildClasses()

    var assertOn = false
    assert { assertOn = true; true }
    if (assertOn) { checkMemo() }

    nUnions
  }

  private def processUnions(): Int = {
    var nUnions = 0
    while (pending.nonEmpty) {
      while (pending.nonEmpty) {
        val (node, eclass) = pending.remove(pending.size - 1)

        val node2 = node.mapChildren(findMut)
        val prev = memo.get(node2)
        memo += node2 -> eclass
        prev match {
          case Some(memoClass) =>
            val (_, didSomething) = union(memoClass, eclass)
            if (didSomething) nUnions += 1
          case None => ()
        }
      }

      while (analysisPending.nonEmpty) {
        val (node, id) = analysisPending.last
        analysisPending.remove((node, id))

        val cid = findMut(id)
        val node_data = analysis.make(this, node)
        val eclass = classes(cid)
        analysis.merge(eclass.data, node_data) match {
          case Some(Equal) | Some(Greater) => ()
          case Some(Less) | None =>
            analysisPending ++= eclass.parents
            analysis.modify(this, cid)
        }
      }
    }

    assert(pending.isEmpty)
    assert(analysisPending.isEmpty)

    nUnions
  }

  private def rebuildClasses(): Int = {
    classesByMatch.values.foreach(ids => ids.clear())

    var trimmed = 0
    for (eclass <- classes.values) {
      val oldNodeCount = eclass.nodeCount()
      // TODO? in egg the nodes are sorted:
      // eclass.nodes.mapInPlace(n => n.updateChildren(findMut))
      // eclass.nodes.sortInPlace()
      // eclass.nodes.distinctInPlace()
      eclass.nodes = eclass.nodes.map(_.mapChildren(findMut)).distinct

      trimmed += oldNodeCount - eclass.nodeCount()

      def add(n: ENode): Unit =
        classesByMatch.getOrElseUpdate(n.matchHash(), HashSet.empty) += eclass.id

      // TODO? in egg the nodes are sorted to remove duplicates where prev.matches(n)
      eclass.nodes.distinctBy(_.matchHash()).foreach(add)
    }

    /* TODO: this is a useless check?
    // check invariants if assertions are enabled
    var assertOn = false
    assert { assertOn = true; true }
    if (assertOn) {
      for (ids <- classesByMatch.values) {
        val unique = ids.toSet
        assert(ids.size == unique.size)
      }
    } */

    trimmed
  }

  private def checkMemo(): Boolean = {
    val testMemo = HashMap.empty[ENode, EClassId]

    for ((id, eclass) <- classes) {
      assert(eclass.id == id);
      for (node <- eclass.nodes) {
        testMemo.get(node) match {
          case None => ()
          case Some(old) => assert(find(old) == find(id))
        }
        testMemo += node -> id
      }
    }

    for ((n, e) <- testMemo) {
      assert(e == find(e))
      assert(memo.get(n).map(find).contains(e))
    }

    true
  }
}
