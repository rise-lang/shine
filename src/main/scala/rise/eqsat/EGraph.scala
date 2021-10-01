package rise.eqsat

import scala.collection.mutable.LinkedHashMap

object EGraph {
  def empty(): EGraph =
    new EGraph(
      analyses = LinkedHashMap.empty,
      typeAnalyses = LinkedHashMap.empty,
      memo = HashMap.empty,
      classes = HashMap.empty,
      unionFind = UnionFind.empty,
      pending = Vec.empty,
      analysisPending = Vec.empty[PendingAnalysis],
      classesByMatch = HashMap.empty,
      hashConses = HashConses.empty(),
      clean = true,
    )
}

sealed trait PendingAnalysis
case class PendingMakeAnalysis(enode: ENode, id: EClassId, t: TypeId) extends PendingAnalysis
case class PendingMergeAnalysis(a: EClassId, aParents: Seq[(ENode, EClassId)],
                                b: EClassId, bParents: Seq[(ENode, EClassId)]) extends PendingAnalysis

class AnalysisData(var refCount: Int, val maps: Any)

/** A data structure to keep track of equalities between expressions.
  * @see [[https://docs.rs/egg/0.6.0/egg/struct.EGraph.html]]
  */
class EGraph(
  // LinkedHashMap to maintain topological ordering according to dependencies
  val analyses: LinkedHashMap[Analysis, AnalysisData],
  val typeAnalyses: LinkedHashMap[TypeAnalysis, AnalysisData],

  var pending: Vec[(ENode, EClassId)],
  var analysisPending: Vec[PendingAnalysis],

  var memo: HashMap[(ENode, TypeId), EClassId],
  var unionFind: UnionFind,
  var classes: HashMap[EClassId, EClass],
  var classesByMatch: HashMap[Int, HashSet[EClassId]],
  var hashConses: HashConses,

  // Whether or not reading operation are allowed on this e-graph.
  // Mutating operations will set this to `false`, and
  // `rebuild` will set it to `true`.
  // Reading operations require this to be `true`.
  var clean: Boolean,
) {
  // TODO: check for dependency cycles?
  def requireAnalyses(x: (Set[Analysis], Set[TypeAnalysis])): Unit = {
    val (as, tas) = x
    as.foreach(requireAnalysis)
    tas.foreach(requireTypeAnalysis)
  }

  def releaseAnalyses(x: (Set[Analysis], Set[TypeAnalysis])): Unit = {
    val (as, tas) = x
    as.foreach(releaseAnalysis)
    tas.foreach(releaseTypeAnalysis)
  }

  def requireAnalysis(a: Analysis): Unit = {
    val ra = a.requiredAnalyses()
    assert(ra._1.isEmpty) // TODO: propagate analysis pending from dependencies?
    requireAnalyses(ra)

    if (!analyses.contains(a)) {
      analyses(a) = new AnalysisData(0, HashMap.empty[EClassId, a.Data])
      a.init(this)
    }
    analyses(a).refCount += 1
  }

  def releaseAnalysis(a: Analysis): Unit = {
    releaseAnalyses(a.requiredAnalyses())

    val data = analyses(a)
    data.refCount -= 1
    if (data.refCount <= 0) {
      analyses.remove(a)
    }
  }

  def requireTypeAnalysis(a: TypeAnalysis): Unit = {
    val depTA = a.requiredTypeAnalyses()
    depTA.foreach(requireTypeAnalysis)

    if (!typeAnalyses.contains(a)) {
      val dataOfNat = HashMap.empty[NatId, a.NatData].asInstanceOf[HashMap[NatId, Any]]
      val dataOfType = HashMap.empty[TypeId, a.TypeData].asInstanceOf[HashMap[TypeId, Any]]
      typeAnalyses(a) = new AnalysisData(0, (dataOfNat, dataOfType))
      TypeAnalysis.update(this, a)
    }
    typeAnalyses(a).refCount += 1
  }

  def releaseTypeAnalysis(a: TypeAnalysis): Unit = {
    val depTA = a.requiredTypeAnalyses()
    depTA.foreach(releaseTypeAnalysis)

    val data = typeAnalyses(a)
    data.refCount -= 1
    if (data.refCount <= 0) {
      typeAnalyses.remove(a)
    }
  }

  // reserved for internal use
  def getAnalysisMap(a: Analysis): HashMap[EClassId, a.Data] =
    analyses(a).maps.asInstanceOf[HashMap[EClassId, a.Data]]

  // reserved for internal use
  def getTypeAnalysisMaps(a: TypeAnalysis): (HashMap[NatId, a.NatData], HashMap[TypeId, a.TypeData]) =
    typeAnalyses(a).maps
      .asInstanceOf[(HashMap[NatId, a.NatData], HashMap[TypeId, a.TypeData])]

  def getAnalysis(a: Analysis): EClassId => a.Data = getAnalysisMap(a)

  def getTypeAnalysis(a: TypeAnalysis): (NatId => a.NatData, TypeId => a.TypeData) =
    getTypeAnalysisMaps(a)

  def nodeCount(): Int =
    classes.map { case (_, c) => c.nodeCount() }.iterator.sum
  def classCount(): Int =
    classes.size

  def find(id: EClassId): EClassId =
    unionFind.find(id)
  def findMut(id: EClassId): EClassId =
    unionFind.findMut(id)

  def get(id: EClassId): EClass =
    classes(find(id))
  def getMut(id: EClassId): EClass =
    classes(findMut(id))

  def apply(id: NatId): NatNode[NatId] =
    hashConses(id)
  def apply(id: DataTypeId): DataTypeNode[NatId, DataTypeId] =
    hashConses(id)
  def apply(id: NotDataTypeId): TypeNode[TypeId, NatId, DataTypeId] =
    hashConses(id)
  def apply(id: TypeId): TypeNode[TypeId, NatId, DataTypeId] =
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
        parents = Vec())

      enode.children().foreach { c =>
        this.getMut(c).parents += enode -> id
      }

      pending += enode -> id // TODO: is this needed?
      analysisPending += PendingMakeAnalysis(enode, id, t)
      classes += id -> eclass
      assert(!memo.contains(enode, t))
      memo += (enode, t) -> id

      // analysis.modify(this, id)
      this.clean = false
      id
    }
  }

  def addExpr(expr: Expr): EClassId =
    add(expr.node.map(addExpr, addNat, addDataType), addType(expr.t))
  def addExpr(expr: ExprWithHashCons): EClassId =
    add(expr.node.map(addExpr, n => n, dt => dt), expr.t)
  def addExpr2(expr: ExprWithHashCons): (ENode, EClassId) = {
    val enode = expr.node.map(addExpr, n => n, dt => dt)
    (enode, add(enode, expr.t))
  }

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
    // analysis.preUnion(this, id1, id2)
    unionFind.union(id1, id2)
    val class2 = classes.remove(id2).get
    val class1 = classes(id1)
    assert(id1 == class1.id)
    assert(class2.t == class1.t)

    pending ++= class2.parents
    analysisPending += PendingMergeAnalysis(
      id1, class1.parents.toSeq, id2, class2.parents.toSeq)

    class1.nodes ++= class2.nodes
    class1.parents ++= class2.parents

    // analysis.modify(this, id1)
    this.clean = false
    (id1, true)
  }

  def rebuild(roots: Seq[EClassId],
              filter: Predicate = NoPredicate()): Int = {
    if (!this.clean) {
      val nUnions = processUnions()
      // val _ = rebuildClasses()
      val _ = this.filter(filter, roots)
      rebuildClassesByMatch()

      assert {
        TypeCheck(this)
        checkMemo()
        true
      }

      this.clean = true
      nUnions
    } else {
      assert(pending.isEmpty)
      assert(analysisPending.isEmpty)
      0
    }
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

      rebuildClasses() // for semigroup analyses

      // NOTE: analysis dependencies should be respected if topological order is maintained
      // TODO: update could also be on-demand / lazy
      typeAnalyses.keysIterator.foreach(ta => TypeAnalysis.update(this, ta))
      analyses.keysIterator.foreach(t => t.update(this))
      analysisPending.clear()
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
      if (eclass.nodes.nonEmpty) {
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
      }
    }

    trimmed
  }

  private def rebuildClassesByMatch(): Unit = {
    classesByMatch.values.foreach(ids => ids.clear())

    for (eclass <- classes.values) {
      // assumption: sorted nodes for optimized search
      def add(n: ENode): Unit =
        classesByMatch.getOrElseUpdate(n.matchHash(), HashSet.empty) += eclass.id

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
  private def filter(predicate: Predicate,
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
      // FIXME: we are currently not updating the analysis data to account for removals
      analyses.keysIterator.foreach(t => t.eliminate(this, toEliminate))
    }

    val eliminatedClasses = originalClassCount - classCount()
    val eliminatedNodes = originalNodeCount - nodeCount()
    println(s"filter eliminated $eliminatedClasses classes" +
      s" and $eliminatedNodes nodes" +
      s" in ${Seq(rt1, rt2, rt3).map(util.prettyTime).mkString(" + ")}")
    (eliminatedClasses, eliminatedNodes)
  }
}
