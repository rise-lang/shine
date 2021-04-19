package rise.eqsat

object Rewrite {
  def init[D](name: String, rule: (Searcher[D], Applier[D])): Rewrite[D] = {
    val (searcher, applier) = rule
    val boundVars = searcher.patternVars()
    val usedVars = applier.patternVars()
    assert(usedVars.forall(boundVars.contains(_)))

    new Rewrite(name, searcher, applier)
  }
}

/** A rewrite rule that searches for a left-hand side using a [[Searcher]],
  * and applies a right-hand side using an [[Applier]].
  */
class Rewrite[Data](val name: String,
                    val searcher: Searcher[Data],
                    val applier: Applier[Data]) {
  def search(egraph: EGraph[Data]): Vec[SearchMatches] =
    searcher.search(egraph)

  def apply(egraph: EGraph[Data], matches: Vec[SearchMatches]): Vec[EClassId] =
    applier.applyMatches(egraph, matches)

  def when(cond: (EGraph[Data], EClassId, Subst) => Boolean): Rewrite[Data] =
    new Rewrite(name, searcher, ConditionalApplier(cond, applier))
}

/** The left-hand side of a [[Rewrite]] rule.
  * A searcher is something that can search the [[EGraph]] for
  * matching substitutions.
  */
trait Searcher[Data] {
  // the variables bound by this searcher
  def patternVars(): Vec[PatternVar]

  // search one eclass, returning None if no matches can be found
  def searchEClass(egraph: EGraph[Data], eclass: EClassId): Option[SearchMatches]

  // search the whole egraph, returning all matches
  def search(egraph: EGraph[Data]): Vec[SearchMatches] = {
    egraph.classes.keys.flatMap(id => searchEClass(egraph, id)).to(Vec)
  }
}

/** The right-hand side of a [[Rewrite]] rule.
  * An applier is anything that can use a [[Subst]] to modify the [[EGraph]].
  */
trait Applier[Data] {
  // the variables used by this applier
  // return an empty Vec to disable checks
  // TODO: update or remove
  def patternVars(): Vec[PatternVar]

  // Apply a single substitition.
  //
  // An `Applier` should only add things to the egraph here,
  // _not_ union them with the id `eclass`.
  //
  // This should return a list of eclasses you'd like to
  // be unioned with `eclass`. There can be zero, one, or many.
  def applyOne(egraph: EGraph[Data], eclass: EClassId, subst: Subst): Vec[EClassId]

  def applyMatches(egraph: EGraph[Data], matches: Vec[SearchMatches]): Vec[EClassId] = {
    val added = Vec.empty[EClassId]
    for (mat <- matches) {
      for (subst <- mat.substs) {
        added ++= applyOne(egraph, mat.eclass, subst)
          .flatMap { id =>
            val (to, didSomething) = egraph.union(id, mat.eclass)
            if (didSomething) { Some(to) } else { None }
          }
      }
    }
    added
  }
}

/** The result of searching over one [[EClass]].
  * Note that multiple substitutions can have been found.
  */
case class SearchMatches(eclass: EClassId, substs: Vec[Subst])

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
    get(key).get

  def shallowClone(): VecMap[K, V] =
    VecMap(vec.clone())
}

object VecMap {
  def empty[K, V]: VecMap[K, V] = VecMap(Vec.empty)
}

/** A substitution mapping variables to their match in the [[EGraph]] */
case class Subst(exprs: VecMap[PatternVar, EClassId],
                 nats: VecMap[NatPatternVar, Nat],
                 types: VecMap[TypePatternVar, Type],
                 datatypes: VecMap[DataTypePatternVar, DataType]) {
  def insert(pv: PatternVar, eclass: EClassId): Option[EClassId] =
    exprs.insert(pv, eclass)
  def insert(nv: NatPatternVar, n: Nat): Option[Nat] =
    nats.insert(nv, n)
  def insert(tv: TypePatternVar, t: Type): Option[Type] =
    types.insert(tv, t)
  def insert(dtv: DataTypePatternVar, dt: DataType): Option[DataType] =
    datatypes.insert(dtv, dt)

  def apply(pv: PatternVar): EClassId =
    exprs(pv)
  def apply(nv: NatPatternVar): Nat =
    nats(nv)
  def apply(tv: TypePatternVar): Type =
    types(tv)
  def apply(dtv: DataTypePatternVar): DataType =
    datatypes(dtv)

  def deepClone(): Subst =
    Subst(exprs.shallowClone(), nats.shallowClone(), types.shallowClone(), datatypes.shallowClone())
}

object Subst {
  def empty: Subst = Subst(VecMap.empty, VecMap.empty, VecMap.empty, VecMap.empty)
}

// note: the condition is more general in `egg`
/** An [[Applier]] that checks a condition before applying another [[Applier]] */
case class ConditionalApplier[D](cond: (EGraph[D], EClassId, Subst) => Boolean,
                                 applier: Applier[D])
  extends Applier[D] {
  override def patternVars(): Vec[PatternVar] =
    applier.patternVars()

  override def applyOne(egraph: EGraph[D], eclass: EClassId, subst: Subst): Vec[EClassId] = {
    if (cond(egraph, eclass, subst)) { applier.applyOne(egraph, eclass, subst) } else { Vec() }
  }
}

/** An [[Applier]] that shifts the DeBruijn indices of a variable.
  * @note It works by extracting an expression from the [[EGraph]] in order to shift it.
  */
case class ShiftedApplier(v: PatternVar, newV: PatternVar,
                          shift: Int, cutoff: Int,
                          applier: Applier[DefaultAnalysisData])
  extends Applier[DefaultAnalysisData] {
  override def patternVars(): Vec[PatternVar] = {
    val vs = Vec(v)
    vs ++= applier.patternVars()
    vs -= newV
    vs
  }

  override def applyOne(egraph: EGraph[DefaultAnalysisData],
                        eclass: EClassId,
                        subst: Subst): Vec[EClassId] = {
    val extract = egraph.getMut(subst(v)).data.extractedExpr
    val shifted = extract.shifted(shift, cutoff)
    val subst2 = subst.deepClone()
    subst2.insert(newV, egraph.addExpr(shifted))
    applier.applyOne(egraph, eclass, subst2)
  }
}

/** An [[Applier]] that performs beta-reduction.
  * @note It works by extracting an expression from the [[EGraph]] in order to beta-reduce it.
  */
case class BetaApplier(body: PatternVar, subs: PatternVar)
  extends Applier[DefaultAnalysisData] {
  override def patternVars(): Vec[PatternVar] = {
    Vec(body, subs)
  }

  override def applyOne(egraph: EGraph[DefaultAnalysisData],
                        eclass: EClassId,
                        subst: Subst): Vec[EClassId] = {
    val bodyEx = egraph.getMut(subst(body)).data.extractedExpr
    val subsEx = egraph.getMut(subst(subs)).data.extractedExpr
    val result = bodyEx.withArgument(subsEx)
    Vec(egraph.addExpr(result))
  }
}
