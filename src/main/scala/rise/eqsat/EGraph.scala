package rise.eqsat

object EGraph {
  def emptyWithAnalysis[ED, ND, TD](analysis: Analysis[ED, ND, TD])
  : EGraph[ED, ND, TD] =
    new EGraph(
      analysis = analysis,
      memo = HashMap.empty,
      classes = HashMap.empty,
      unionFind = UnionFind.empty,
      pending = Vec.empty,
      analysisPending = {
        import Node.{ordering, natIdOrdering, dataTypeIdOrdering, eclassIdOrdering}
        collection.mutable.TreeSet.empty[(ENode, EClassId)]
      },
      classesByMatch = HashMap.empty,
      hashConses = HashConses.emptyWithAnalysis(analysis),
    )
}

/** A data structure to keep track of equalities between expressions.
  * @see [[https://docs.rs/egg/0.6.0/egg/struct.EGraph.html]]
  */
class EGraph[ED, ND, TD](
  val analysis: Analysis[ED, ND, TD],
  var pending: Vec[(ENode, EClassId)],
  // NOTE: TreeSet has faster pop than HashSet,
  // can also look into something like IndexSet
  // https://docs.rs/indexmap/1.0.2/indexmap/set/struct.IndexSet.html#method.pop
  var analysisPending: collection.mutable.TreeSet[(ENode, EClassId)],

  var memo: HashMap[(ENode, TypeId), EClassId],
  var unionFind: UnionFind,
  var classes: HashMap[EClassId, EClass[ED]],
  var classesByMatch: HashMap[Int, HashSet[EClassId]],
  var hashConses: HashConses[ND, TD],
) {
  def nodeCount(): Int =
    classes.map { case (_, c) => c.nodeCount() }.iterator.sum
  def classCount(): Int =
    classes.size

  def find(id: EClassId): EClassId =
    unionFind.find(id)
  def findMut(id: EClassId): EClassId =
    unionFind.findMut(id)

  def get(id: EClassId): EClass[ED] =
    classes(find(id))
  def getMut(id: EClassId): EClass[ED] =
    classes(findMut(id))

  def apply(id: NatId): (NatNode[NatId], ND) =
    hashConses(id)
  def apply(id: DataTypeId): (DataTypeNode[NatId, DataTypeId], TD) =
    hashConses(id)
  def apply(id: NotDataTypeId): (TypeNode[TypeId, NatId, DataTypeId], TD) =
    hashConses(id)
  def apply(id: TypeId): (TypeNode[TypeId, NatId, DataTypeId], TD) =
    hashConses(id)

  // returns the canonicalized enode and its eclass if it has one
  def lookup(enode: ENode, t: TypeId): (ENode, Option[EClassId]) = {
    val enode2 = enode.mapChildren(find)
    val id = memo.get(enode2, t)
    (enode2, id.map(find))
  }

  def makeEmptyEClass(t: TypeId): EClassId = {
    val newId = unionFind.makeSet()
    val newEclass = new EClass(
      id = newId,
      t = t,
      nodes = Vec(),
      data = analysis.empty(this, t),
      parents = Vec())
    classes += newId -> newEclass
    newId
  }

  def add(n: ENode, t: TypeId): EClassId = {
    val (enode, optec) = lookup(n, t)
    optec.getOrElse {
      val id = unionFind.makeSet()
      val eclass = new EClass(
        id = id,
        t = t,
        nodes = Vec(enode),
        data = analysis.make(this, enode, t),
        parents = Vec())

      enode.children().foreach { c =>
        this.getMut(c).parents += enode -> id
      }

      pending += enode -> id // TODO: is this needed?
      classes += id -> eclass
      assert(!memo.contains(enode, t))
      memo += (enode, t) -> id

      analysis.modify(this, id)
      id
    }
  }

  def addExpr(expr: Expr): EClassId =
    add(expr.node.map(addExpr, addNat, addDataType), addType(expr.t))
  def addExpr(expr: ExprWithHashCons): EClassId =
    add(expr.node.map(addExpr, n => n, dt => dt), expr.t)

  def lookupExpr(expr: Expr): Option[EClassId] =
    lookup(expr.node.map(
      e => lookupExpr(e).getOrElse(return None),
      addNat, addDataType
    ), addType(expr.t))._2

  def add(n: NatNode[NatId]): NatId =
    hashConses.add(n)
  def addNat(n: Nat): NatId =
    hashConses.addNat(n)

  def add(dt: DataTypeNode[NatId, DataTypeId]): DataTypeId =
    hashConses.add(dt)
  def addDataType(dt: DataType): DataTypeId =
    hashConses.addDataType(dt)

  def add(t: TypeNode[TypeId, NatId, DataTypeId]): TypeId =
    hashConses.add(t)
  def addType(t: Type): TypeId =
    hashConses.addType(t)

  // checks whether two expressions are equivalent
  // returns a list of eclasses that represent both expressions
  def equivs(e1: Expr, e2: Expr): Vec[EClassId] = {
    val shc = SubstHashCons.empty
    val matches1 = Pattern.fromExpr(e1).compile().search(this, shc)
    val matches2 = Pattern.fromExpr(e2).compile().search(this, shc)
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
    assert(class2.t == class1.t)

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

  def rebuild(roots: Seq[EClassId],
              filter: Predicate[ED, ND, TD] = NoPredicate()): Int = {
    val nUnions = processUnions()
    val _ = this.filter(filter, roots)
    val _ = rebuildClasses()

    assert {
      TypeCheck(this)
      checkMemo()
      true
    }

    nUnions
  }

  private def processUnions(): Int = {
    var nUnions = 0
    while (pending.nonEmpty || analysisPending.nonEmpty) {
      while (pending.nonEmpty) {
        val (node, eclass) = pending.remove(pending.size - 1)
        val t = get(eclass).t

        val node2 = node.mapChildren(findMut)
        val prev = memo.get(node2, t)
        memo += (node2, t) -> eclass
        prev match {
          case Some(memoClass) =>
            val (_, didSomething) = union(memoClass, eclass)
            if (didSomething) nUnions += 1
          case None => ()
        }
      }

      while (analysisPending.nonEmpty) {
        // note: using .last is really slow, traversing all the map
        val (node, id) = analysisPending.head
        analysisPending.remove((node, id))

        val cid = findMut(id)
        val eclass = classes(cid)
        val node_data = analysis.make(this, node, eclass.t)
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
    import Node.{ordering, eclassIdOrdering, natIdOrdering, dataTypeIdOrdering}
    classesByMatch.values.foreach(ids => ids.clear())

    var trimmed = 0
    for (eclass <- classes.values) {
      val oldNodeCount = eclass.nodeCount()

      // sort nodes for optimized search
      val sortedNodes = eclass.nodes.mapInPlace(n => n.mapChildren(findMut))
        .sorted
      // remove duplicates
      eclass.nodes.clear()
      eclass.nodes += sortedNodes.head
      for (nn <- sortedNodes.view.tail) {
        if (nn != eclass.nodes.last) {
          eclass.nodes += nn
        }
      }
      // eclass.nodes = eclass.nodes.map(_.mapChildren(findMut)).distinct

      trimmed += oldNodeCount - eclass.nodeCount()

      def add(n: ENode): Unit =
        classesByMatch.getOrElseUpdate(n.matchHash(), HashSet.empty) += eclass.id

      // TODO? in egg the nodes are sorted and duplicates where prev.matches(n) are not added
      // eclass.nodes.foreach(add)
      eclass.nodes.headOption match {
        case None =>
        case Some(first) =>
          add(first)

          var prev = first
          for (n <- eclass.nodes.view.tail) {
            if (!prev.matches(n)) {
              add(n)
            }
            prev = n
          }
      }
    }

    trimmed
  }

  private def checkMemo(): Boolean = {
    val testMemo = HashMap.empty[(ENode, TypeId), EClassId]

    for ((id, eclass) <- classes) {
      assert(eclass.id == id)
      for (node <- eclass.nodes) {
        testMemo.get(node, eclass.t) match {
          case None => ()
          case Some(old) => assert(find(old) == find(id))
        }
        testMemo += (node, eclass.t) -> id
      }
    }

    for ((n, e) <- testMemo) {
      assert(e == find(e))
      assert(memo.get(n).map(find).contains(e))
    }

    true
  }

  // returns (eliminatedClasses, eliminatedNodes)
  private def filter(predicate: Predicate[ED, ND, TD],
                     roots: Seq[EClassId]): (Int, Int) = {
    assert(pending.isEmpty)
    assert(analysisPending.isEmpty)

    val rootsCanonical = roots.map(findMut).toSet
    def isRoot(id: EClassId): Boolean = rootsCanonical(id)

    val originalClassCount = classCount()
    val originalNodeCount = nodeCount()

    // 1. collect classes to eliminate
    val toEliminate = HashSet.empty[EClassId]
    val (rt1, _) = util.time {
      predicate match {
        case NoPredicate() =>
        case _ =>
          predicate.start(this, roots)
          for (eclass <- classes.values) {
            assert(eclass.id == findMut(eclass.id))
            if (!predicate(this, eclass)) {
              assert(!isRoot(eclass.id))
              toEliminate += eclass.id
            }
          }
          predicate.stop()
      }
    }

    /* TODO? and if no directional rewrite was triggered
    if (toEliminate.isEmpty) {
      return (0, 0)
    }
   */

    def eclassToEliminate(id: EClassId): Boolean =
      toEliminate(findMut(id))
    def enodeToEliminate(n: ENode): Boolean =
      n.children().exists(eclassToEliminate)

    // 2. also eliminate classes that would become
    // unreachable or dead ends
    val (rt2, _) = util.time {
      var spread = true
      while (spread) {
        spread = false

        for (eclass <- classes.values) {
          if (!toEliminate(eclass.id)) {
            val unreachable = !isRoot(eclass.id) &&
              eclass.parents.forall { case (pn, pid) => eclassToEliminate(pid) || enodeToEliminate(pn) }
            val deadEnd = eclass.nodes.forall(enodeToEliminate)
            if (unreachable || deadEnd) {
              assert(!isRoot(eclass.id))
              toEliminate += eclass.id
              spread = true
            }
          }
        }
      }
    }

    if (toEliminate.isEmpty) {
      return (0, 0)
    }

    // 3. perform the actual elimination
    val (rt3, _) = util.time {
      classes.filterInPlace { case (id, _) =>
        !toEliminate(id)
      }
      classes.foreach { case (_, eclass) =>
        eclass.parents.filterInPlace { case (pn, pid) =>
          !(eclassToEliminate(pid) || enodeToEliminate(pn))
        }
        eclass.nodes.filterInPlace { c =>
          !enodeToEliminate(c)
        }
      }
      memo.filterInPlace { case (_, id) =>
        !eclassToEliminate(id)
      }
    }

    // FIXME: we are currently not updating the analysis data
    //  to account for removals

    val eliminatedClasses = originalClassCount - classCount()
    val eliminatedNodes = originalNodeCount - nodeCount()
    println(s"filter eliminated $eliminatedClasses classes" +
      s" and $eliminatedNodes nodes" +
      s" in ${Seq(rt1, rt2, rt3).map(util.prettyTime).mkString(" + ")}")
    (eliminatedClasses, eliminatedNodes)
  }
}
