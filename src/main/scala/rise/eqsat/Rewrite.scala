package rise.eqsat

import rise.core.{types => rct}

object Rewrite {
  def init[D](name: String, rule: (Searcher[D], Applier[D])): Rewrite[D] = {
    val (searcher, applier) = rule
    assert {
      val boundVars = searcher.patternVars()
      val usedVars = applier.patternVars()
      val delta = usedVars diff boundVars
      if (delta.nonEmpty) {
        throw new Exception(s"used vars which are not bound: $delta")
      }; true
    }

    new Rewrite(name, searcher, applier)
  }
}

/** A rewrite rule that searches for a left-hand side using a [[Searcher]],
  * and applies a right-hand side using an [[Applier]].
  */
class Rewrite[Data](val name: String,
                    val searcher: Searcher[Data],
                    val applier: Applier[Data]) {
  override def toString: String = s"$name:\n$searcher\n  -->\n$applier"

  def search(egraph: EGraph[Data]): Vec[SearchMatches] =
    searcher.search(egraph)

  def apply(egraph: EGraph[Data], matches: Vec[SearchMatches]): Vec[EClassId] =
    applier.applyMatches(egraph, matches)

  // TODO: remove this, change to named free test
  def when(cond: (EGraph[Data], EClassId, Subst) => Boolean): Rewrite[Data] =
    new Rewrite(name, searcher, ConditionalApplier(cond, Set(), applier))
}

/** The left-hand side of a [[Rewrite]] rule.
  * A searcher is something that can search the [[EGraph]] for
  * matching substitutions.
  */
trait Searcher[Data] {
  // the variables bound by this searcher
  def patternVars(): Set[Any]

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
  // return empty to disable checks
  def patternVars(): Set[Any]

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
    get(key).getOrElse(throw new Exception(s"could not find $key"))

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
                                 condPatternVars: Set[Any],
                                 applier: Applier[D])
  extends Applier[D] {
  override def toString: String = s"$applier when $cond"

  override def patternVars(): Set[Any] =
    applier.patternVars() ++ condPatternVars

  override def applyOne(egraph: EGraph[D], eclass: EClassId, subst: Subst): Vec[EClassId] = {
    if (cond(egraph, eclass, subst)) { applier.applyOne(egraph, eclass, subst) } else { Vec() }
  }
}

/** An [[Applier]] that shifts the DeBruijn indices of a variable */
case class ShiftedApplier(v: PatternVar, newV: PatternVar,
                          shift: Expr.Shift, cutoff: Expr.Shift,
                          applier: Applier[DefaultAnalysisData])
  extends Applier[DefaultAnalysisData] {
  override def patternVars(): Set[Any] =
    applier.patternVars() - newV + v

  override def applyOne(egraph: EGraph[DefaultAnalysisData],
                        eclass: EClassId,
                        subst: Subst): Vec[EClassId] = {
    val subst2 = subst.deepClone()
    subst2.insert(newV, EClass.shifted(subst(v), shift, cutoff, egraph))
    applier.applyOne(egraph, eclass, subst2)
  }
}

/** An [[Applier]] that shifts the DeBruijn indices of a variable.
  * @note It works by extracting an expression from the [[EGraph]] in order to shift it.
  */
case class ShiftedExtractApplier(v: PatternVar, newV: PatternVar,
                                 shift: Expr.Shift, cutoff: Expr.Shift,
                                 applier: Applier[DefaultAnalysisData])
  extends Applier[DefaultAnalysisData] {
  override def patternVars(): Set[Any] =
    applier.patternVars() - newV + v

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

/** An [[Applier]] that checks whether a shifted variable is equal to another
  * @note It works by extracting an expression from the [[EGraph]] in order to shift it.
  */
object ShiftedCheckApplier {
  def apply(v: PatternVar, v2: PatternVar,
            shift: Expr.Shift, cutoff: Expr.Shift,
            applier: Applier[DefaultAnalysisData]): Applier[DefaultAnalysisData] =
    ConditionalApplier({ case (egraph, _, subst) =>
      val extract = egraph.getMut(subst(v)).data.extractedExpr
      val shifted = extract.shifted(shift, cutoff)
      val expected = egraph.getMut(subst(v2)).data.extractedExpr
      shifted == expected
    }, Set(v, v2), applier)
}

/** An [[Applier]] that shifts the DeBruijn indices of a nat variable */
case class ShiftedNatApplier[D](v: NatPatternVar, newV: NatPatternVar,
                                shift: Nat.Shift, cutoff: Nat.Shift,
                                applier: Applier[D])
  extends Applier[D] {
  override def patternVars(): Set[Any] =
    applier.patternVars() - newV + v

  override def applyOne(egraph: EGraph[D],
                        eclass: EClassId,
                        subst: Subst): Vec[EClassId] = {
    val nat = subst(v)
    val shifted = nat.shifted(shift, cutoff)
    val subst2 = subst.deepClone()
    subst2.insert(newV, shifted)
    applier.applyOne(egraph, eclass, subst2)
  }
}

/** An [[Applier]] that checks whether a shifted nat variable is equal to another */
object ShiftedNatCheckApplier {
  def apply(v: NatPatternVar, v2: NatPatternVar,
            shift: Nat.Shift, cutoff: Nat.Shift,
            applier: Applier[DefaultAnalysisData]): Applier[DefaultAnalysisData] =
    ConditionalApplier({ case (_, _, subst) =>
      val nat = subst(v)
      val shifted = nat.shifted(shift, cutoff)
      val expected = subst(v2)
      shifted == expected
    }, Set(v, v2), applier)
}

/** An [[Applier]] that shifts the DeBruijn indices of a data type variable */
case class ShiftedDataTypeApplier[D](v: DataTypePatternVar, newV: DataTypePatternVar,
                                     shift: Type.Shift, cutoff: Type.Shift,
                                     applier: Applier[D])
  extends Applier[D] {
  override def patternVars(): Set[Any] =
    applier.patternVars() - newV + v

  override def applyOne(egraph: EGraph[D],
                        eclass: EClassId,
                        subst: Subst): Vec[EClassId] = {
    val dt = subst(v)
    val shifted = dt.shifted(shift, cutoff)
    val subst2 = subst.deepClone()
    subst2.insert(newV, shifted)
    applier.applyOne(egraph, eclass, subst2)
  }
}

/** An [[Applier]] that checks whether a shifted nat variable is equal to another */
object ShiftedDataTypeCheckApplier {
  def apply(v: DataTypePatternVar, v2: DataTypePatternVar,
            shift: Type.Shift, cutoff: Type.Shift,
            applier: Applier[DefaultAnalysisData]): Applier[DefaultAnalysisData] =
    ConditionalApplier({ case (_, _, subst) =>
      val dt = subst(v)
      val shifted = dt.shifted(shift, cutoff)
      val expected = subst(v2)
      shifted == expected
    }, Set(v, v2), applier)
}

/** An [[Applier]] that shifts the DeBruijn indices of a type variable */
case class ShiftedTypeApplier[D](v: TypePatternVar, newV: TypePatternVar,
                                 shift: Type.Shift, cutoff: Type.Shift,
                                 applier: Applier[D])
  extends Applier[D] {
  override def patternVars(): Set[Any] =
    applier.patternVars() - newV + v

  override def applyOne(egraph: EGraph[D],
                        eclass: EClassId,
                        subst: Subst): Vec[EClassId] = {
    val t = subst(v)
    val shifted = t.shifted(shift, cutoff)
    val subst2 = subst.deepClone()
    subst2.insert(newV, shifted)
    applier.applyOne(egraph, eclass, subst2)
  }
}

/** An [[Applier]] that checks whether a shifted nat variable is equal to another */
object ShiftedTypeCheckApplier {
  def apply(v: TypePatternVar, v2: TypePatternVar,
            shift: Type.Shift, cutoff: Type.Shift,
            applier: Applier[DefaultAnalysisData]): Applier[DefaultAnalysisData] =
    ConditionalApplier({ case (_, _, subst) =>
      val t = subst(v)
      val shifted = t.shifted(shift, cutoff)
      val expected = subst(v2)
      shifted == expected
    }, Set(v, v2), applier)
}

/** An [[Applier]] that performs beta-reduction.
  * @note It works by extracting an expression from the [[EGraph]] in order to beta-reduce it.
  */
case class BetaExtractApplier(body: PatternVar, subs: PatternVar)
  extends Applier[DefaultAnalysisData] {
  override def patternVars(): Set[Any] =
    Set(body, subs)

  override def applyOne(egraph: EGraph[DefaultAnalysisData],
                        eclass: EClassId,
                        subst: Subst): Vec[EClassId] = {
    val bodyEx = egraph.getMut(subst(body)).data.extractedExpr
    val subsEx = egraph.getMut(subst(subs)).data.extractedExpr
    val result = bodyEx.withArgument(subsEx)
    Vec(egraph.addExpr(result))
  }
}

/** An [[Applier]] that performs beta-reduction.
  * @note It works by directly transforming [[EClass]]es.
  */
case class BetaApplier(body: PatternVar, subs: PatternVar)
  extends Applier[DefaultAnalysisData] {
  override def patternVars(): Set[Any] =
    Set(body, subs)

  override def applyOne(egraph: EGraph[DefaultAnalysisData],
                        eclass: EClassId,
                        subst: Subst): Vec[EClassId] = {
    Vec(EClass.withArgument(subst(body), subst(subs), egraph))
  }
}

/** An [[Applier]] that performs beta-reduction for nat-dependent functions.
  * @note It works by directly transforming [[EClass]]es.
  */
case class BetaNatApplier(body: PatternVar, subs: NatPatternVar)
  extends Applier[DefaultAnalysisData] {
  override def patternVars(): Set[Any] =
    Set(body, subs)

  override def applyOne(egraph: EGraph[DefaultAnalysisData],
                        eclass: EClassId,
                        subst: Subst): Vec[EClassId] = {
    Vec(EClass.withNatArgument(subst(body), subst(subs), egraph))
  }
}

/** An [[Applier]] that performs beta-reduction for nat-dependent functions.
  * @note It works by extracting an expression from the [[EGraph]] in order to beta-reduce it.
  */
case class BetaNatExtractApplier(body: PatternVar, subs: NatPatternVar)
  extends Applier[DefaultAnalysisData] {
  override def patternVars(): Set[Any] =
    Set(body, subs)

  override def applyOne(egraph: EGraph[DefaultAnalysisData],
                        eclass: EClassId,
                        subst: Subst): Vec[EClassId] = {
    val bodyEx = egraph.getMut(subst(body)).data.extractedExpr
    val subsNat = subst(subs)
    val result = bodyEx.withNatArgument(subsNat)
    Vec(egraph.addExpr(result))
  }
}

/** An [[Applier]] that checks whether a nat variable is equal to a nat pattern */
object ComputeNatCheckApplier {
  def apply[D](v: NatPatternVar, expected: NatPattern,
               applier: Applier[D]): Applier[D] =
    ConditionalApplier({ case (_, _, subst) =>
      ComputeNat.toNamed(v, subst) == ComputeNat.toNamed(expected, subst)
    }, expected.patternVars() + v, applier)
}

/** An [[Applier]] that computes a nat variable according to a nat pattern */
case class ComputeNatApplier[D](v: NatPatternVar, value: NatPattern,
                                applier: Applier[D]) extends Applier[D] {

  override def patternVars(): Set[Any] =
    applier.patternVars() - v ++ value.patternVars()

  override def applyOne(egraph: EGraph[D],
                        eclass: EClassId,
                        subst: Subst): Vec[EClassId] = {
    val actualValue = Nat.fromNamedGeneric(
      ComputeNat.toNamed(value, subst), ni => ni.name.drop(1).toInt)
    val subst2 = subst.deepClone()
    subst2.insert(v, actualValue)
    applier.applyOne(egraph, eclass, subst2)
  }
}

private object ComputeNat {
  def toNamed(n: NatPattern, subst: Subst): rct.Nat = {
    import arithexpr.arithmetic._

    n match {
      case NatPatternAny => throw new Exception("")
      case pv: NatPatternVar => Nat.toNamedGeneric(subst(pv), i => rct.NatIdentifier(s"n$i"))
      case NatPatternNode(node) => node match {
        case NatVar(index) => rct.NatIdentifier(s"n$index")
        case NatCst(value) => Cst(value)
        case NatNegInf => NegInf
        case NatPosInf => PosInf
        case NatAdd(a, b) => toNamed(a, subst) + toNamed(b, subst)
        case NatMul(a, b) => toNamed(a, subst) * toNamed(b, subst)
        case NatPow(b, e) => toNamed(b, subst).pow(toNamed(e, subst))
        case NatMod(a, b) => toNamed(a, subst) % toNamed(b, subst)
        case NatIntDiv(a, b) => toNamed(a, subst) / toNamed(b, subst)
      }
    }
  }
}