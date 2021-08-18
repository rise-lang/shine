package rise.eqsat

/** A substitution mapping variables to their match in the [[EGraph]].
  * It uses hash-consing for structural sharing amongst many substitutions.
  */
case class Subst(exprs: SubstId[PatternVar, EClassId],
                 nats: SubstId[NatPatternVar, NatId],
                 types: SubstId[TypePatternVar, TypeId],
                 dataTypes: SubstId[DataTypePatternVar, DataTypeId]) {
  private def orNotFound[PV, ID](pv: PV, opt: Option[ID]): ID =
    opt.getOrElse(throw new Exception(s"could not find $pv"))

  def apply(pv: PatternVar, shc: SubstHashCons): EClassId =
    orNotFound(pv, shc.findVar(pv, exprs, shc.getExpr))
  def apply(pv: NatPatternVar, shc: SubstHashCons): NatId =
    orNotFound(pv, shc.findVar(pv, nats, shc.getNat))
  def apply(pv: TypePatternVar, shc: SubstHashCons): TypeId =
    orNotFound(pv, shc.findVar(pv, types, shc.getType))
  def apply(pv: DataTypePatternVar, shc: SubstHashCons): DataTypeId =
    orNotFound(pv, shc.findVar(pv, dataTypes, shc.getDataType))
}

sealed trait SubstNode[PV, ID]
case class SubstNil[PV, ID]() extends SubstNode[PV, ID]
case class SubstCons[PV, ID](pv: PV, id: ID, s: SubstId[PV, ID]) extends SubstNode[PV, ID]

case class SubstId[PV, ID](i: Int)

object SubstHashCons {
  def empty: SubstHashCons = SubstHashCons(
    exprs = HashCons.empty,
    nats = HashCons.empty,
    types = HashCons.empty,
    dataTypes = HashCons.empty,
  )
}

case class SubstHashCons(
  exprs: HashCons[SubstNode[PatternVar, EClassId], SubstId[PatternVar, EClassId], ()],
  nats: HashCons[SubstNode[NatPatternVar, NatId], SubstId[NatPatternVar, NatId], ()],
  types: HashCons[SubstNode[TypePatternVar, TypeId], SubstId[TypePatternVar, TypeId], ()],
  dataTypes: HashCons[SubstNode[DataTypePatternVar, DataTypeId], SubstId[DataTypePatternVar, DataTypeId], ()])
{
  def getExpr(id: SubstId[PatternVar, EClassId]): SubstNode[PatternVar, EClassId] =
    exprs.get(id)._1
  def getNat(id: SubstId[NatPatternVar, NatId]): SubstNode[NatPatternVar, NatId] =
    nats.get(id)._1
  def getType(id: SubstId[TypePatternVar, TypeId]): SubstNode[TypePatternVar, TypeId] =
    types.get(id)._1
  def getDataType(id: SubstId[DataTypePatternVar, DataTypeId]): SubstNode[DataTypePatternVar, DataTypeId] =
    dataTypes.get(id)._1

  def findVar[PV, ID](pv: PV, s: SubstId[PV, ID],
                      get: SubstId[PV, ID] => SubstNode[PV, ID]): Option[ID] =
    get(s) match {
      case SubstNil() => None
      case SubstCons(pv2, id, s2) => if (pv == pv2) {
        Some(id)
      } else {
        findVar(pv, s2, get)
      }
    }

  private def addExpr(n: SubstNode[PatternVar, EClassId]): SubstId[PatternVar, EClassId] =
    exprs.add(n, _ => (), SubstId[PatternVar, EClassId])
  private def addNat(n: SubstNode[NatPatternVar, NatId]): SubstId[NatPatternVar, NatId] =
    nats.add(n, _ => (), SubstId[NatPatternVar, NatId])
  private def addType(n: SubstNode[TypePatternVar, TypeId]): SubstId[TypePatternVar, TypeId] =
    types.add(n, _ => (), SubstId[TypePatternVar, TypeId])
  private def addDataType(n: SubstNode[DataTypePatternVar, DataTypeId]): SubstId[DataTypePatternVar, DataTypeId] =
    dataTypes.add(n, _ => (), SubstId[DataTypePatternVar, DataTypeId])

  private def makeSubst[PV, ID](it: Iterator[(PV, ID)],
                             addOne: SubstNode[PV, ID] => SubstId[PV, ID]): SubstId[PV, ID] =
    it.nextOption() match {
      case None => addOne(SubstNil())
      case Some((pv, id)) => addOne(SubstCons(pv, id, makeSubst(it, addOne)))
    }

  def exprSubst(it: Iterator[(PatternVar, EClassId)]): SubstId[PatternVar, EClassId] =
    makeSubst(it, addExpr)
  def natSubst(it: Iterator[(NatPatternVar, NatId)]): SubstId[NatPatternVar, NatId] =
    makeSubst(it, addNat)
  def typeSubst(it: Iterator[(TypePatternVar, TypeId)]): SubstId[TypePatternVar, TypeId] =
    makeSubst(it, addType)
  def dataTypeSubst(it: Iterator[(DataTypePatternVar, DataTypeId)]): SubstId[DataTypePatternVar, DataTypeId] =
    makeSubst(it, addDataType)

  private def substInsert[PV, ID](pv: PV, id: ID, s: SubstId[PV, ID],
                                  addOne: SubstNode[PV, ID] => SubstId[PV, ID],
                                  get: SubstId[PV, ID] => SubstNode[PV, ID]): SubstId[PV, ID] = {
    assert(findVar(pv, s, get).isEmpty)
    addOne(SubstCons(pv, id, s))
  }

  def substInsert(pv: PatternVar, id: EClassId, subst: Subst): Subst =
    subst.copy(exprs = substInsert(pv, id, subst.exprs, addExpr, getExpr))
  def substInsert(pv: NatPatternVar, id: NatId, subst: Subst): Subst =
    subst.copy(nats = substInsert(pv, id, subst.nats, addNat, getNat))
  def substInsert(pv: TypePatternVar, id: TypeId, subst: Subst): Subst =
    subst.copy(types = substInsert(pv, id, subst.types, addType, getType))
  def substInsert(pv: DataTypePatternVar, id: DataTypeId, subst: Subst): Subst =
    subst.copy(dataTypes = substInsert(pv, id, subst.dataTypes, addDataType, getDataType))
}

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
