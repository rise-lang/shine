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
      analysisPending = HashSet.empty,
      classesByMatch = HashMap.empty,

      nats = HashCons.empty,
      dataTypes = HashCons.empty,
      types = HashCons.empty
    )
}

/** A data structure to keep track of equalities between expressions.
  * @see [[https://docs.rs/egg/0.6.0/egg/struct.EGraph.html]]
  */
class EGraph[ED, ND, TD](
  val analysis: Analysis[ED, ND, TD],
  var pending: Vec[(ENode, EClassId)],
  var analysisPending: HashSet[(ENode, EClassId)],

  var memo: HashMap[(ENode, TypeId), EClassId],
  var unionFind: UnionFind,
  var classes: HashMap[EClassId, EClass[ED]],
  var classesByMatch: HashMap[Int, HashSet[EClassId]],

  var nats: HashCons[NatNode[NatId], NatId, ND],
  var dataTypes: HashCons[DataTypeNode[NatId, DataTypeId], DataTypeId, TD],
  var types: HashCons[TypeNode[TypeId, NatId, DataTypeId], NotDataTypeId, TD],
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
    nats.get(id)
  def apply(id: DataTypeId): (DataTypeNode[NatId, DataTypeId], TD) =
    dataTypes.get(id)
  def apply(id: NotDataTypeId): (TypeNode[TypeId, NatId, DataTypeId], TD) =
    types.get(id)
  def apply(id: TypeId): (TypeNode[TypeId, NatId, DataTypeId], TD) =
    id match {
      case i: DataTypeId => apply(i)
      case i: NotDataTypeId => apply(i)
    }

  // returns the canonicalized enode and its eclass if it has one
  def lookup(enode: ENode, t: TypeId): (ENode, Option[EClassId]) = {
    val enode2 = enode.mapChildren(find)
    val id = memo.get(enode2, t)
    (enode2, id.map(find))
  }

  def makeEmptyEClass(simplifiedT: TypeId): EClassId = {
    val newId = unionFind.makeSet()
    val newEclass = new EClass(
      id = newId,
      t = simplifiedT,
      nodes = Vec(),
      data = analysis.empty(this, simplifiedT),
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

  def add(n: NatNode[NatId]): NatId = {
    import rise.core.{types => rct}
    import arithexpr.arithmetic._

    // TODO: avoid code duplication with other named conversions
    def toNamed(n: NatNode[NatId]): rct.Nat =
      n match {
        case NatVar(index) => rct.NatIdentifier(s"n$index")
        case NatCst(value) => Cst(value)
        case NatNegInf => NegInf
        case NatPosInf => PosInf
        case NatAdd(a, b) => idToNamed(a) + idToNamed(b)
        case NatMul(a, b) => idToNamed(a) * idToNamed(b)
        case NatPow(b, e) => idToNamed(b).pow(idToNamed(e))
        case NatMod(a, b) => idToNamed(a) % idToNamed(b)
        case NatIntDiv(a, b) => idToNamed(a) / idToNamed(b)
      }

    def idToNamed(id: NatId): rct.Nat =
      toNamed(this(id)._1)

    def fromNamed(n: rct.Nat): NatNode[NatId] = {
      n match {
        case i: rct.NatIdentifier => NatVar(i.name.drop(1).toInt)
        case PosInf => NatPosInf
        case NegInf => NatNegInf
        case Cst(c) => NatCst(c)
        case Sum(Nil) => NatCst(0)
        case Sum(t +: ts) => ts.foldRight(fromNamed(t)) { case (t, acc) =>
          NatAdd(add(fromNamed(t)), add(acc))
        }
        case Prod(Nil) => NatCst(1)
        case Prod(t +: ts) => ts.foldRight(fromNamed(t)) { case (t, acc) =>
          NatMul(add(fromNamed(t)), add(acc))
        }
        case Pow(b, e) =>
          NatPow(add(fromNamed(b)), add(fromNamed(e)))
        case Mod(a, b) =>
          NatMod(add(fromNamed(a)), add(fromNamed(b)))
        case IntDiv(a, b) =>
          NatIntDiv(add(fromNamed(a)), add(fromNamed(b)))
        case _ => throw new Exception(s"no support for $n")
      }
    }

    nats.addWithSimplification(n, NatId, n => fromNamed(toNamed(n)), n => analysis.makeNat(this, n))
  }

  def addNat(n: Nat): NatId = {
    def rec(n: Nat): NatId =
      add(n.node.map(rec))

    rec(Nat.simplify(n))
  }

  def add(dt: DataTypeNode[NatId, DataTypeId]): DataTypeId =
    dataTypes.add(dt, dt => analysis.makeDataType(this, dt), DataTypeId)

  def addDataType(dt: DataType): DataTypeId =
    add(dt.node.map(addNat, addDataType))

  def add(t: TypeNode[TypeId, NatId, DataTypeId]): TypeId = {
    t match {
      case dt: DataTypeNode[NatId, DataTypeId] => add(dt)
      case _ => types.add(t, t => analysis.makeType(this, t), NotDataTypeId)
    }
  }

  def addType(t: Type): TypeId =
    add(t.node.map(addType, addNat, addDataType))

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

  def rebuild(): Int = {
    val nUnions = processUnions()
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
    while (pending.nonEmpty) {
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
      // .distinctBy(_.matchHash())
      eclass.nodes.foreach(add)
    }

    trimmed
  }

  private def checkMemo(): Boolean = {
    val testMemo = HashMap.empty[(ENode, TypeId), EClassId]

    for ((id, eclass) <- classes) {
      assert(eclass.id == id);
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
}
