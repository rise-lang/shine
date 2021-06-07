package rise.eqsat

import rise.core.{types => rct}

object Rewrite {
  def init[ED, ND, DT](name: String, rule: (Searcher[ED, ND, DT], Applier[ED, ND, DT])): Rewrite[ED, ND, DT] = {
    val (searcher, applier) = rule
    assert {
      val boundVars = searcher.patternVars()
      val usedVars = applier.patternVars()
      val delta = usedVars diff boundVars
      if (delta.nonEmpty) {
        throw new Exception(s"used vars which are not bound: $delta")
      }; true
    }

    new Rewrite(name, searcher, applier, isDirected = false)
  }
}

/** A rewrite rule that searches for a left-hand side using a [[Searcher]],
  * and applies a right-hand side using an [[Applier]].
  */
class Rewrite[ED, ND, DT](val name: String,
                          val searcher: Searcher[ED, ND, DT],
                          val applier: Applier[ED, ND, DT],
                          val isDirected: Boolean) {
  override def toString: String = s"$name:\n$searcher\n  -->\n$applier"

  def search(egraph: EGraph[ED, ND, DT],
             shc: SubstHashCons): Vec[SearchMatches] =
    searcher.search(egraph, shc)

  // the substitution insides `matches` may be modified
  def apply(egraph: EGraph[ED, ND, DT],
            shc: SubstHashCons,
            matches: Vec[SearchMatches]): Vec[EClassId] =
    applier.applyMatches(egraph, shc, matches, isDirected)

  def directed(): Rewrite[ED, ND, DT] = new Rewrite(name, searcher, applier, true)
}

/** The left-hand side of a [[Rewrite]] rule.
  * A searcher is something that can search the [[EGraph]] for
  * matching substitutions.
  */
trait Searcher[ED, ND, TD] {
  // the variables bound by this searcher
  def patternVars(): Set[Any]

  // search one eclass, returning None if no matches can be found
  def searchEClass(egraph: EGraph[ED, ND, TD],
                   shc: SubstHashCons,
                   eclass: EClassId): Option[SearchMatches]

  // search the whole egraph, returning all matches
  def search(egraph: EGraph[ED, ND, TD], shc: SubstHashCons): Vec[SearchMatches] = {
    egraph.classes.keys.flatMap(id => searchEClass(egraph, shc, id)).to(Vec)
  }
}

/** The right-hand side of a [[Rewrite]] rule.
  * An applier is anything that can use a [[Subst]] to modify the [[EGraph]].
  */
trait Applier[ED, ND, TD] {
  // the variables used by this applier
  // return empty to disable checks
  def patternVars(): Set[Any]

  // Apply a single substitition.
  //
  // An `Applier` should only add things to the egraph here,
  // _not_ union them with the id `eclass`.
  //
  // This should return a list of eclasses you'd like to
  // be unioned with `eclass`. There can be zero, one, or many.
  //
  // `subst` may be modified
  def applyOne(egraph: EGraph[ED, ND, TD],
               eclass: EClassId,
               shc: SubstHashCons,
               subst: Subst): Vec[EClassId]

  // the substitutions inside `matches` may be modified
  def applyMatches(egraph: EGraph[ED, ND, TD],
                   shc: SubstHashCons,
                   matches: Vec[SearchMatches],
                   directed: Boolean = false): Vec[EClassId] = {
    val added = Vec.empty[EClassId]
    for (mat <- matches) {
      val rootsToDelete = HashSet.empty[ENode]

      for ((rootNode, subst) <- mat.substs) {
        val res = applyOne(egraph, mat.eclass, shc, subst)
        if (directed && res.nonEmpty && !res.contains(mat.eclass)) {
          rootsToDelete += rootNode
            .getOrElse(throw new Exception("directed rewrite without root match"))
        }
        added ++= res.iterator.flatMap { id =>
            val (to, didSomething) = egraph.union(id, mat.eclass)
            if (didSomething) { Some(to) } else { None }
          }
      }

      // FIXME: not accounting for analysis data here
      if (rootsToDelete.nonEmpty) {
        egraph.getMut(mat.eclass).nodes.filterInPlace(!rootsToDelete(_))
      }
    }
    added
  }
}

/** The result of searching over one [[EClass]].
  * Note that multiple substitutions can have been found.
  */
case class SearchMatches(eclass: EClassId, substs: Vec[(Option[ENode], Subst)])

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

/** A substitution mapping variables to their match in the [[EGraph]] */
/*
case class Subst(exprs: VecMap[PatternVar, EClassId],
                 nats: VecMap[NatPatternVar, NatId],
                 types: VecMap[TypePatternVar, TypeId],
                 datatypes: VecMap[DataTypePatternVar, DataTypeId]) {
  def insert(pv: PatternVar, eclass: EClassId): Option[EClassId] =
    exprs.insert(pv, eclass)
  def insert(nv: NatPatternVar, n: NatId): Option[NatId] =
    nats.insert(nv, n)
  def insert(tv: TypePatternVar, t: TypeId): Option[TypeId] =
    types.insert(tv, t)
  def insert(dtv: DataTypePatternVar, dt: DataTypeId): Option[DataTypeId] =
    datatypes.insert(dtv, dt)

  def apply(pv: PatternVar): EClassId =
    exprs(pv)
  def apply(nv: NatPatternVar): NatId =
    nats(nv)
  def apply(tv: TypePatternVar): TypeId =
    types(tv)
  def apply(dtv: DataTypePatternVar): DataTypeId =
    datatypes(dtv)

  def deepClone(): Subst =
    Subst(exprs.shallowClone(), nats.shallowClone(), types.shallowClone(), datatypes.shallowClone())
}

object Subst {
  def empty: Subst = Subst(VecMap.empty, VecMap.empty, VecMap.empty, VecMap.empty)
}
*/

// note: the condition is more general in `egg`
/** An [[Applier]] that checks a condition before applying another [[Applier]] */
case class ConditionalApplier[ED, ND, DT](cond: (EGraph[ED, ND, DT], EClassId, SubstHashCons, Subst) => Boolean,
                                          condPatternVars: Set[Any],
                                          applier: Applier[ED, ND, DT])
  extends Applier[ED, ND, DT] {
  override def toString: String = s"$applier when $cond"

  override def patternVars(): Set[Any] =
    applier.patternVars() ++ condPatternVars

  override def applyOne(egraph: EGraph[ED, ND, DT],
                        eclass: EClassId,
                        shc: SubstHashCons,
                        subst: Subst): Vec[EClassId] = {
    if (cond(egraph, eclass, shc, subst)) { applier.applyOne(egraph, eclass, shc, subst) } else { Vec() }
  }
}

/** An [[Applier]] that shifts the DeBruijn indices of a variable */
case class ShiftedApplier(v: PatternVar, newV: PatternVar,
                          shift: Expr.Shift, cutoff: Expr.Shift,
                          applier: DefaultAnalysis.Applier)
  extends DefaultAnalysis.Applier {
  override def patternVars(): Set[Any] =
    applier.patternVars() - newV + v

  override def applyOne(egraph: DefaultAnalysis.EGraph,
                        eclass: EClassId,
                        shc: SubstHashCons,
                        subst: Subst): Vec[EClassId] = {
    ??? // subst.insert(newV, EClass.shifted(subst(v), shift, cutoff, egraph)
    applier.applyOne(egraph, eclass, shc, subst)
  }
}

/** An [[Applier]] that shifts the DeBruijn indices of a variable.
  * @note It works by extracting an expression from the [[EGraph]] in order to shift it.
  */
case class ShiftedExtractApplier(v: PatternVar, newV: PatternVar,
                                 shift: Expr.Shift, cutoff: Expr.Shift,
                                 applier: DefaultAnalysis.Applier)
  extends DefaultAnalysis.Applier {
  override def patternVars(): Set[Any] =
    applier.patternVars() - newV + v

  override def applyOne(egraph: DefaultAnalysis.EGraph,
                        eclass: EClassId,
                        shc: SubstHashCons,
                        subst: Subst): Vec[EClassId] = {
    val extract = egraph.getMut(subst(v, shc)).data.extractedExpr
    val shifted = extract.shifted(egraph, shift, cutoff)
    val subst2 = shc.substInsert(newV, egraph.addExpr(shifted), subst)
    applier.applyOne(egraph, eclass, shc, subst2)
  }
}

/** An [[Applier]] that checks whether a shifted variable is equal to another
  * @note It works by extracting an expression from the [[EGraph]] in order to shift it.
  */
object ShiftedCheckApplier {
  def apply(v: PatternVar, v2: PatternVar,
            shift: Expr.Shift, cutoff: Expr.Shift,
            applier: DefaultAnalysis.Applier): DefaultAnalysis.Applier =
    ConditionalApplier({ case (egraph, _, shc, subst) =>
      val extract = egraph.getMut(subst(v, shc)).data.extractedExpr
      val shifted = extract.shifted(egraph, shift, cutoff)
      val expected = egraph.getMut(subst(v2, shc)).data.extractedExpr
      shifted == expected
    }, Set(v, v2), applier)
}

/** An [[Applier]] that shifts the DeBruijn indices of a nat variable */
case class ShiftedNatApplier[ED, ND, DT](v: NatPatternVar, newV: NatPatternVar,
                                         shift: Nat.Shift, cutoff: Nat.Shift,
                                         applier: Applier[ED, ND, DT])
  extends Applier[ED, ND, DT] {
  override def patternVars(): Set[Any] =
    applier.patternVars() - newV + v

  override def applyOne(egraph: EGraph[ED, ND, DT],
                        eclass: EClassId,
                        shc: SubstHashCons,
                        subst: Subst): Vec[EClassId] = {
    val nat = subst(v, shc)
    val shifted = NodeSubs.Nat.shifted(egraph, nat, shift, cutoff)
    val subst2 = shc.substInsert(newV, shifted, subst)
    applier.applyOne(egraph, eclass, shc, subst2)
  }
}

/** An [[Applier]] that checks whether a shifted nat variable is equal to another */
object ShiftedNatCheckApplier {
  def apply(v: NatPatternVar, v2: NatPatternVar,
            shift: Nat.Shift, cutoff: Nat.Shift,
            applier: DefaultAnalysis.Applier): DefaultAnalysis.Applier =
    ConditionalApplier({ case (egraph, _, shc, subst) =>
      val nat = subst(v, shc)
      val shifted = NodeSubs.Nat.shifted(egraph, nat, shift, cutoff)
      val expected = subst(v2, shc)
      shifted == expected
    }, Set(v, v2), applier)
}

/** An [[Applier]] that shifts the DeBruijn indices of a data type variable */
case class ShiftedDataTypeApplier[ED, ND, DT](v: DataTypePatternVar, newV: DataTypePatternVar,
                                              shift: Type.Shift, cutoff: Type.Shift,
                                              applier: Applier[ED, ND, DT])
  extends Applier[ED, ND, DT] {
  override def patternVars(): Set[Any] =
    applier.patternVars() - newV + v

  override def applyOne(egraph: EGraph[ED, ND, DT],
                        eclass: EClassId,
                        shc: SubstHashCons,
                        subst: Subst): Vec[EClassId] = {
    val dt = subst(v, shc)
    val shifted = NodeSubs.DataType.shifted(egraph, dt, shift, cutoff)
    val subst2 = shc.substInsert(newV, shifted, subst)
    applier.applyOne(egraph, eclass, shc, subst2)
  }
}

/** An [[Applier]] that checks whether a shifted nat variable is equal to another */
object ShiftedDataTypeCheckApplier {
  def apply(v: DataTypePatternVar, v2: DataTypePatternVar,
            shift: Type.Shift, cutoff: Type.Shift,
            applier: DefaultAnalysis.Applier): DefaultAnalysis.Applier =
    ConditionalApplier({ case (egraph, _, shc, subst) =>
      val dt = subst(v, shc)
      val shifted = NodeSubs.DataType.shifted(egraph, dt, shift, cutoff)
      val expected = subst(v2, shc)
      shifted == expected
    }, Set(v, v2), applier)
}

/** An [[Applier]] that shifts the DeBruijn indices of a type variable */
case class ShiftedTypeApplier[ED, ND, DT](v: TypePatternVar, newV: TypePatternVar,
                                          shift: Type.Shift, cutoff: Type.Shift,
                                          applier: Applier[ED, ND, DT])
  extends Applier[ED, ND, DT] {
  override def patternVars(): Set[Any] =
    applier.patternVars() - newV + v

  override def applyOne(egraph: EGraph[ED, ND, DT],
                        eclass: EClassId,
                        shc: SubstHashCons,
                        subst: Subst): Vec[EClassId] = {
    val t = subst(v, shc)
    val shifted = NodeSubs.Type.shifted(egraph, t, shift, cutoff)
    val subst2 = shc.substInsert(newV, shifted, subst)
    applier.applyOne(egraph, eclass, shc, subst2)
  }
}

/** An [[Applier]] that checks whether a shifted nat variable is equal to another */
object ShiftedTypeCheckApplier {
  def apply(v: TypePatternVar, v2: TypePatternVar,
            shift: Type.Shift, cutoff: Type.Shift,
            applier: DefaultAnalysis.Applier): DefaultAnalysis.Applier =
    ConditionalApplier({ case (egraph, _, shc, subst) =>
      val t = subst(v, shc)
      val shifted = NodeSubs.Type.shifted(egraph, t, shift, cutoff)
      val expected = subst(v2, shc)
      shifted == expected
    }, Set(v, v2), applier)
}

/** An [[Applier]] that performs beta-reduction.
  * @note It works by extracting an expression from the [[EGraph]] in order to beta-reduce it.
  */
case class BetaExtractApplier(body: PatternVar, subs: PatternVar)
  extends DefaultAnalysis.Applier {
  override def patternVars(): Set[Any] =
    Set(body, subs)

  override def applyOne(egraph: DefaultAnalysis.EGraph,
                        eclass: EClassId,
                        shc: SubstHashCons,
                        subst: Subst): Vec[EClassId] = {
    val bodyEx = egraph.getMut(subst(body, shc)).data.extractedExpr
    val subsEx = egraph.getMut(subst(subs, shc)).data.extractedExpr
    val result = bodyEx.withArgument(egraph, subsEx)
    Vec(egraph.addExpr(result))
  }
}

/** An [[Applier]] that performs beta-reduction.
  * @note It works by directly transforming [[EClass]]es.
  */
case class BetaApplier(body: PatternVar, subs: PatternVar)
  extends DefaultAnalysis.Applier {
  override def patternVars(): Set[Any] =
    Set(body, subs)

  override def applyOne(egraph: DefaultAnalysis.EGraph,
                        eclass: EClassId,
                        shc: SubstHashCons,
                        subst: Subst): Vec[EClassId] = {
    ??? // Vec(EClass.withArgument(subst(body), subst(subs), egraph))
  }
}

/** An [[Applier]] that performs beta-reduction for nat-dependent functions.
  * @note It works by directly transforming [[EClass]]es.
  */
case class BetaNatApplier(body: PatternVar, subs: NatPatternVar)
  extends DefaultAnalysis.Applier {
  override def patternVars(): Set[Any] =
    Set(body, subs)

  override def applyOne(egraph: DefaultAnalysis.EGraph,
                        eclass: EClassId,
                        shc: SubstHashCons,
                        subst: Subst): Vec[EClassId] = {
    ??? // Vec(EClass.withNatArgument(subst(body), subst(subs), egraph))
  }
}

/** An [[Applier]] that performs beta-reduction for nat-dependent functions.
  * @note It works by extracting an expression from the [[EGraph]] in order to beta-reduce it.
  */
case class BetaNatExtractApplier(body: PatternVar, subs: NatPatternVar)
  extends DefaultAnalysis.Applier {
  override def patternVars(): Set[Any] =
    Set(body, subs)

  override def applyOne(egraph: DefaultAnalysis.EGraph,
                        eclass: EClassId,
                        shc: SubstHashCons,
                        subst: Subst): Vec[EClassId] = {
    val bodyEx = egraph.getMut(subst(body, shc)).data.extractedExpr
    val subsNat = subst(subs, shc)
    val result = bodyEx.withNatArgument(egraph, subsNat)
    Vec(egraph.addExpr(result))
  }
}

/** An [[Applier]] that checks whether a nat variable is equal to a nat pattern */
object ComputeNatCheckApplier {
  def apply[ED, ND, TD](v: NatPatternVar, expected: NatPattern,
                        applier: Applier[ED, ND, TD]): Applier[ED, ND, TD] =
    ConditionalApplier({ case (egraph, _, shc, subst) =>
      // TODO: can we be more efficient here?
      ComputeNat.toNamed(egraph, v, shc, subst) == ComputeNat.toNamed(egraph, expected, shc, subst)
    }, expected.patternVars() + v, applier)
}

/** An [[Applier]] that computes a nat variable according to a nat pattern */
case class ComputeNatApplier[ED, ND, TD](v: NatPatternVar, value: NatPattern,
                                         applier: Applier[ED, ND, TD]) extends Applier[ED, ND, TD] {

  override def patternVars(): Set[Any] =
    applier.patternVars() - v ++ value.patternVars()

  override def applyOne(egraph: EGraph[ED, ND, TD],
                        eclass: EClassId,
                        shc: SubstHashCons,
                        subst: Subst): Vec[EClassId] = {
    // TODO: can we be more efficient here?
    val actualValue = Nat.fromNamedGeneric(
      ComputeNat.toNamed(egraph, value, shc, subst), ni => ni.name.drop(1).toInt)
    val subst2 = shc.substInsert(v, egraph.addNat(actualValue), subst)
    applier.applyOne(egraph, eclass, shc, subst2)
  }
}

private object ComputeNat {
  import arithexpr.arithmetic._

  def toNamed[ED, ND, TD](egraph: EGraph[ED, ND, TD],
                          n: NatPattern,
                          shc: SubstHashCons,
                          subst: Subst): rct.Nat = {
    n match {
      case NatPatternAny => throw new Exception("")
      case pv: NatPatternVar => toNamed(egraph, subst(pv, shc))
      case NatPatternNode(node) => node match {
        case NatVar(index) => rct.NatIdentifier(s"n$index")
        case NatCst(value) => Cst(value)
        case NatNegInf => NegInf
        case NatPosInf => PosInf
        case NatAdd(a, b) => toNamed(egraph, a, shc, subst) + toNamed(egraph, b, shc, subst)
        case NatMul(a, b) => toNamed(egraph, a, shc, subst) * toNamed(egraph, b, shc, subst)
        case NatPow(b, e) => toNamed(egraph, b, shc, subst).pow(toNamed(egraph, e, shc, subst))
        case NatMod(a, b) => toNamed(egraph, a, shc, subst) % toNamed(egraph, b, shc, subst)
        case NatIntDiv(a, b) => toNamed(egraph, a, shc, subst) / toNamed(egraph, b, shc, subst)
      }
    }
  }

  def toNamed[ED, ND, TD](egraph: EGraph[ED, ND, TD], id: NatId): rct.Nat = {
    egraph(id)._1 match {
      case NatVar(index) => rct.NatIdentifier(s"n$index")
      case NatCst(value) => Cst(value)
      case NatNegInf => NegInf
      case NatPosInf => PosInf
      case NatAdd(a, b) => toNamed(egraph, a) + toNamed(egraph, b)
      case NatMul(a, b) => toNamed(egraph, a) * toNamed(egraph, b)
      case NatPow(b, e) => toNamed(egraph, b).pow(toNamed(egraph, e))
      case NatMod(a, b) => toNamed(egraph, a) % toNamed(egraph, b)
      case NatIntDiv(a, b) => toNamed(egraph, a) / toNamed(egraph, b)
    }
  }
}
