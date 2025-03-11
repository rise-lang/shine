package rise.eqsat

/** A collection of substitutions. */
trait Substs {
  /** A substitution mapping variables to their match in the [[EGraph]]. */
  type Subst

  def get(pv: PatternVar, subst: Subst): EClassId
  def get(pv: NatPatternVar, subst: Subst): NatId
  def get(pv: TypePatternVar, subst: Subst): TypeId
  def get(pv: DataTypePatternVar, subst: Subst): DataTypeId
  def get(pv: AddressPatternVar, subst: Subst): Address

  def insert(pv: PatternVar, id: EClassId, subst: Subst): Subst
  def insert(pv: NatPatternVar, id: NatId, subst: Subst): Subst
  def insert(pv: TypePatternVar, id: TypeId, subst: Subst): Subst
  def insert(pv: DataTypePatternVar, id: DataTypeId, subst: Subst): Subst
  def insert(pv: AddressPatternVar, id: Address, subst: Subst): Subst

  def create(pvs: Iterator[(PatternVar, EClassId)],
             nvs: Iterator[(NatPatternVar, NatId)],
             tvs: Iterator[(TypePatternVar, TypeId)],
             dvs: Iterator[(DataTypePatternVar, DataTypeId)],
             avs: Iterator[(AddressPatternVar, Address)]): Subst
}

// TODO? could use a packed Vec[Option[V]] where K is the index
case class VecMap[K, V](vec: Vec[(K, V)]) {
  // insert a mapping, returning the old value if present
  def insert(key: K, value: V): Option[V] = {
    for (((v, ec), i) <- vec.zipWithIndex) {
      if (v == key) {
        vec.update(i, key -> value)
        return Some(ec)
      }
    }
    vec += key -> value
    None
  }

  def get(key: K): Option[V] =
    vec.find(_._1 == key).map(_._2)

  def apply(key: K): V =
    get(key).getOrElse(throw new Exception(s"could not find $key"))

  def shallowClone(): VecMap[K, V] =
    VecMap(vec.clone())

  def size: Int = vec.size
}

object VecMap {
  def empty[K, V]: VecMap[K, V] = VecMap(Vec.empty)
}

/** A substitution mapping variables to their match in the [[EGraph]].
  * It uses simple vec maps. */
case class SubstVM(exprs: VecMap[PatternVar, EClassId],
                   nats: VecMap[NatPatternVar, NatId],
                   types: VecMap[TypePatternVar, TypeId],
                   datatypes: VecMap[DataTypePatternVar, DataTypeId],
                   addresses: VecMap[AddressPatternVar, Address]) {
  def insert(pv: PatternVar, eclass: EClassId): Option[EClassId] =
    exprs.insert(pv, eclass)
  def insert(nv: NatPatternVar, n: NatId): Option[NatId] =
    nats.insert(nv, n)
  def insert(tv: TypePatternVar, t: TypeId): Option[TypeId] =
    types.insert(tv, t)
  def insert(dtv: DataTypePatternVar, dt: DataTypeId): Option[DataTypeId] =
    datatypes.insert(dtv, dt)
  def insert(av: AddressPatternVar, a: Address): Option[Address] =
    addresses.insert(av, a)

  def apply(pv: PatternVar): EClassId =
    exprs(pv)
  def apply(nv: NatPatternVar): NatId =
    nats(nv)
  def apply(tv: TypePatternVar): TypeId =
    types(tv)
  def apply(dtv: DataTypePatternVar): DataTypeId =
    datatypes(dtv)
  def apply(av: AddressPatternVar): Address =
    addresses(av)

  def deepClone(): SubstVM =
    SubstVM(exprs.shallowClone(), nats.shallowClone(),
      types.shallowClone(), datatypes.shallowClone(), addresses.shallowClone())
}

object SubstVM {
  def empty: SubstVM = SubstVM(VecMap.empty, VecMap.empty, VecMap.empty, VecMap.empty, VecMap.empty)
}

object SubstsVM extends Substs {
  override type Subst = SubstVM

  override def get(pv: PatternVar, subst: SubstVM): EClassId = subst(pv)
  override def get(pv: NatPatternVar, subst: SubstVM): NatId = subst(pv)
  override def get(pv: TypePatternVar, subst: SubstVM): TypeId = subst(pv)
  override def get(pv: DataTypePatternVar, subst: SubstVM): DataTypeId = subst(pv)
  override def get(pv: AddressPatternVar, subst: SubstVM): Address = subst(pv)

  // FIXME: avoid cloning all the time ...
  private def withClone(subst: SubstVM, f: SubstVM => Unit): SubstVM = {
    val c = subst.deepClone()
    f(c)
    c
  }

  override def insert(pv: PatternVar, id: EClassId, subst: SubstVM): SubstVM =
    withClone(subst, _.insert(pv, id))
  override def insert(pv: NatPatternVar, id: NatId, subst: SubstVM): SubstVM =
    withClone(subst, _.insert(pv, id))
  override def insert(pv: TypePatternVar, id: TypeId, subst: SubstVM): SubstVM =
    withClone(subst, _.insert(pv, id))
  override def insert(pv: DataTypePatternVar, id: DataTypeId, subst: SubstVM): SubstVM =
    withClone(subst, _.insert(pv, id))
  override def insert(pv: AddressPatternVar, id: Address, subst: SubstVM): SubstVM =
    withClone(subst, _.insert(pv, id))

  override def create(pvs: Iterator[(PatternVar, EClassId)],
                      nvs: Iterator[(NatPatternVar, NatId)],
                      tvs: Iterator[(TypePatternVar, TypeId)],
                      dvs: Iterator[(DataTypePatternVar, DataTypeId)],
                      avs: Iterator[(AddressPatternVar, Address)]): SubstVM = {
    SubstVM(VecMap(pvs.to(Vec)), VecMap(nvs.to(Vec)),
      VecMap(tvs.to(Vec)), VecMap(dvs.to(Vec)), VecMap(avs.to(Vec)))
  }
}

/** A substitution mapping variables to their match in the [[EGraph]].
  * It uses hash-consing for structural sharing amongst many substitutions.
  * @fixme sharing depends on insertion order and is not leveraged to reduce runtime (only memory usage)
  */
case class SubstHC(exprs: SubstId[PatternVar, EClassId],
                   nats: SubstId[NatPatternVar, NatId],
                   types: SubstId[TypePatternVar, TypeId],
                   dataTypes: SubstId[DataTypePatternVar, DataTypeId],
                   addrs: SubstId[AddressPatternVar, Address])

sealed trait SubstNode[PV, ID]
case class SubstNil[PV, ID]() extends SubstNode[PV, ID]
case class SubstCons[PV, ID](pv: PV, id: ID, s: SubstId[PV, ID]) extends SubstNode[PV, ID]

case class SubstId[PV, ID](i: Int)

object SubstsHC {
  def empty: SubstsHC = SubstsHC(
    exprs = HashCons.empty,
    nats = HashCons.empty,
    types = HashCons.empty,
    dataTypes = HashCons.empty,
    addrs = HashCons.empty,
  )
}

case class SubstsHC(
                   exprs: HashCons[SubstNode[PatternVar, EClassId], SubstId[PatternVar, EClassId]],
                   nats: HashCons[SubstNode[NatPatternVar, NatId], SubstId[NatPatternVar, NatId]],
                   types: HashCons[SubstNode[TypePatternVar, TypeId], SubstId[TypePatternVar, TypeId]],
                   dataTypes: HashCons[SubstNode[DataTypePatternVar, DataTypeId], SubstId[DataTypePatternVar, DataTypeId]],
                   addrs: HashCons[SubstNode[AddressPatternVar, Address], SubstId[AddressPatternVar, Address]])
  extends Substs
{
  override type Subst = SubstHC

  def getExpr(id: SubstId[PatternVar, EClassId]): SubstNode[PatternVar, EClassId] =
    exprs.get(id)
  def getNat(id: SubstId[NatPatternVar, NatId]): SubstNode[NatPatternVar, NatId] =
    nats.get(id)
  def getType(id: SubstId[TypePatternVar, TypeId]): SubstNode[TypePatternVar, TypeId] =
    types.get(id)
  def getDataType(id: SubstId[DataTypePatternVar, DataTypeId]): SubstNode[DataTypePatternVar, DataTypeId] =
    dataTypes.get(id)
  def getAddr(id: SubstId[AddressPatternVar, Address]): SubstNode[AddressPatternVar, Address] =
    addrs.get(id)

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
    exprs.add(n, SubstId[PatternVar, EClassId])
  private def addNat(n: SubstNode[NatPatternVar, NatId]): SubstId[NatPatternVar, NatId] =
    nats.add(n, SubstId[NatPatternVar, NatId])
  private def addType(n: SubstNode[TypePatternVar, TypeId]): SubstId[TypePatternVar, TypeId] =
    types.add(n, SubstId[TypePatternVar, TypeId])
  private def addDataType(n: SubstNode[DataTypePatternVar, DataTypeId]): SubstId[DataTypePatternVar, DataTypeId] =
    dataTypes.add(n, SubstId[DataTypePatternVar, DataTypeId])
  private def addAddr(n: SubstNode[AddressPatternVar, Address]): SubstId[AddressPatternVar, Address] =
    addrs.add(n, SubstId[AddressPatternVar, Address])

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
  def addrSubst(it: Iterator[(AddressPatternVar, Address)]): SubstId[AddressPatternVar, Address] =
    makeSubst(it, addAddr)

  private def substInsert[PV, ID](pv: PV, id: ID, s: SubstId[PV, ID],
                                  addOne: SubstNode[PV, ID] => SubstId[PV, ID],
                                  get: SubstId[PV, ID] => SubstNode[PV, ID]): SubstId[PV, ID] = {
    assert(findVar(pv, s, get).isEmpty)
    addOne(SubstCons(pv, id, s))
  }

  override def insert(pv: PatternVar, id: EClassId, subst: SubstHC): SubstHC =
    subst.copy(exprs = substInsert(pv, id, subst.exprs, addExpr, getExpr))
  override def insert(pv: NatPatternVar, id: NatId, subst: SubstHC): SubstHC =
    subst.copy(nats = substInsert(pv, id, subst.nats, addNat, getNat))
  override def insert(pv: TypePatternVar, id: TypeId, subst: SubstHC): SubstHC =
    subst.copy(types = substInsert(pv, id, subst.types, addType, getType))
  override def insert(pv: DataTypePatternVar, id: DataTypeId, subst: SubstHC): SubstHC =
    subst.copy(dataTypes = substInsert(pv, id, subst.dataTypes, addDataType, getDataType))
  override def insert(pv: AddressPatternVar, id: Address, subst: SubstHC): SubstHC =
    subst.copy(addrs = substInsert(pv, id, subst.addrs, addAddr, getAddr))

  private def orNotFound[PV, ID](pv: PV, opt: Option[ID]): ID =
    opt.getOrElse(throw new Exception(s"could not find $pv"))

  override def get(pv: PatternVar, subst: SubstHC): EClassId =
    orNotFound(pv, findVar(pv, subst.exprs, getExpr))
  override def get(pv: NatPatternVar, subst: SubstHC): NatId =
    orNotFound(pv, findVar(pv, subst.nats, getNat))
  override def get(pv: TypePatternVar, subst: SubstHC): TypeId =
    orNotFound(pv, findVar(pv, subst.types, getType))
  override def get(pv: DataTypePatternVar, subst: SubstHC): DataTypeId =
    orNotFound(pv, findVar(pv, subst.dataTypes, getDataType))
  override def get(pv: AddressPatternVar, subst: SubstHC): Address =
    orNotFound(pv, findVar(pv, subst.addrs, getAddr))

  override def create(pvs: Iterator[(PatternVar, EClassId)],
                      nvs: Iterator[(NatPatternVar, NatId)],
                      tvs: Iterator[(TypePatternVar, TypeId)],
                      dvs: Iterator[(DataTypePatternVar, DataTypeId)],
                      avs: Iterator[(AddressPatternVar, Address)]): SubstHC = {
    SubstHC(exprSubst(pvs), natSubst(nvs),
      typeSubst(tvs), dataTypeSubst(dvs), addrSubst(avs))
  }
}