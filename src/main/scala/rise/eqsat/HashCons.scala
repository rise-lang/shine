package rise.eqsat

object HashConses {
  def emptyWithAnalysis[ND, TD](analysis: Analysis[_, ND, TD]): HashConses[ND, TD] =
    HashConses(
      analysis = analysis,
      nats = HashCons.empty,
      dataTypes = HashCons.empty,
      types = HashCons.empty
    )
}

case class HashConses[ND, TD](
  analysis: Analysis[_, ND, TD],
  nats: HashCons[NatNode[NatId], NatId, ND],
  dataTypes: HashCons[DataTypeNode[NatId, DataTypeId], DataTypeId, TD],
  types: HashCons[TypeNode[TypeId, NatId, DataTypeId], NotDataTypeId, TD]
) {
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

    // FIXME: simplifying recursively on every add might be too expensive?
    nats.addWithSimplification(n, NatId, n => fromNamed(toNamed(n)), n => analysis.makeNat(this, n))
  }

  def addNat(n: Nat): NatId = {
    def rec(n: Nat): NatId =
      add(n.node.map(rec))

    rec(Nat.simplify(n))
  }

  def add(dt: DataTypeNode[NatId, DataTypeId]): DataTypeId =
    dataTypes.add(dt, dt => analysis.makeType(this, dt), DataTypeId)

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
}

object HashCons {
  def empty[Node, Id, Data]: HashCons[Node, Id, Data] =
    new HashCons(HashMap(), HashMap())
}

class HashCons[Node, Id, Data](
  var memo: HashMap[Node, Id],
  var nodes: HashMap[Id, (Node, Data)],
) {
  def add(node: Node, data: Node => Data, makeId: Int => Id): Id = {
    memo.getOrElseUpdate(node, {
      val id = makeId(nodes.size)
      nodes += id -> (node, data(node))
      memo += node -> id
      id
    })
  }

  def addWithSimplification(node: Node, makeId: Int => Id,
                            simplify: Node => Node, data: Node => Data): Id = {
    memo.get(node) match {
      case Some(id) => id
      case None =>
        val simplified = simplify(node) // this may add more hash-consed values
        add(simplified, data, makeId)
    }
  }

  def get(id: Id): (Node, Data) = nodes(id)
}
