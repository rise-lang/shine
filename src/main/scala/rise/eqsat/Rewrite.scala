package rise.eqsat

import rise.core.{types => rct}

object Rewrite {
  def init(name: String, rule: (Searcher, Applier)): Rewrite = {
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
class Rewrite(val name: String,
              val searcher: Searcher,
              val applier: Applier) {
  override def toString: String = s"$name:\n$searcher\n  -->\n$applier"

  def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    applier.requiredAnalyses()

  def search(egraph: EGraph,
             shc: Substs): Vec[SearchMatches[shc.Subst]] =
    searcher.search(egraph, shc)

  // the substitution insides `matches` may be modified
  def apply(egraph: EGraph,
            shc: Substs)(
            matches: Vec[SearchMatches[shc.Subst]]): Vec[EClassId] =
    applier.applyMatches(egraph, shc)(matches)
}

/** The left-hand side of a [[Rewrite]] rule.
  * A searcher is something that can search the [[EGraph]] for
  * matching substitutions.
  */
trait Searcher {
  // the variables bound by this searcher
  def patternVars(): Set[Any]

  // search one eclass, returning None if no matches can be found
  def searchEClass(egraph: EGraph,
                   shc: Substs,
                   eclass: EClassId): Option[SearchMatches[shc.Subst]]

  // search the whole egraph, returning all matches
  def search(egraph: EGraph, shc: Substs): Vec[SearchMatches[shc.Subst]] = {
    egraph.classes.keys.flatMap(id => searchEClass(egraph, shc, id)).to(Vec)
  }
}

/** The right-hand side of a [[Rewrite]] rule.
  * An applier is anything that can use a [[Subst]] to modify the [[EGraph]].
  */
trait Applier {
  // the variables used by this applier
  // return empty to disable checks
  def patternVars(): Set[Any]

  def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis])

  // Apply a single substitition.
  //
  // An `Applier` should only add things to the egraph here,
  // _not_ union them with the id `eclass`.
  //
  // This should return a list of eclasses you'd like to
  // be unioned with `eclass`. There can be zero, one, or many.
  //
  // `subst` may be modified
  def applyOne(egraph: EGraph,
               eclass: EClassId,
               shc: Substs)(
               subst: shc.Subst): Vec[EClassId]

  // the substitutions inside `matches` may be modified
  def applyMatches(egraph: EGraph,
                   shc: Substs)(
                   matches: Vec[SearchMatches[shc.Subst]]): Vec[EClassId] = {
    val added = Vec.empty[EClassId]
    for (mat <- matches) {
      for (subst <- mat.substs) {
        val res = applyOne(egraph, mat.eclass, shc)(subst)
        added ++= res.iterator.flatMap { id =>
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
case class SearchMatches[Subst](eclass: EClassId, substs: Vec[Subst])

// note: the condition is more general in `egg`
/** An [[Applier]] that checks a condition before applying another [[Applier]] */
abstract class ConditionalApplier(condPatternVars: Set[Any],
                                  condRequiredAnalyses: (Set[Analysis], Set[TypeAnalysis]),
                                  applier: Applier)
  extends Applier {
  override def toString: String = s"$applier when .."

  def cond(egraph: EGraph, id: EClassId, substs: Substs)(subst: substs.Subst): Boolean

  override def patternVars(): Set[Any] =
    applier.patternVars() ++ condPatternVars

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) = {
    Analysis.mergeRequired(condRequiredAnalyses, applier.requiredAnalyses())
  }

  override def applyOne(egraph: EGraph,
                        eclass: EClassId,
                        shc: Substs)(
                        subst: shc.Subst): Vec[EClassId] = {
    if (cond(egraph, eclass, shc)(subst)) { applier.applyOne(egraph, eclass, shc)(subst) } else { Vec() }
  }
}

/** An [[Applier]] that shifts the DeBruijn indices of a variable */
case class ShiftedApplier(v: PatternVar, newV: PatternVar,
                          shift: Expr.Shift, cutoff: Expr.Shift,
                          applier: Applier)
  extends Applier {
  override def patternVars(): Set[Any] =
    applier.patternVars() - newV + v

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) = ???

  override def applyOne(egraph: EGraph,
                        eclass: EClassId,
                        shc: Substs)(
                        subst: shc.Subst): Vec[EClassId] = {
    ??? // subst.insert(newV, EClass.shifted(subst(v), shift, cutoff, egraph)
    applier.applyOne(egraph, eclass, shc)(subst)
  }
}

/** An [[Applier]] that shifts the DeBruijn indices of a variable.
  * @note It works by extracting an expression from the [[EGraph]] in order to shift it.
  */
case class ShiftedExtractApplier(v: PatternVar, newV: PatternVar,
                                 shift: Expr.Shift, cutoff: Expr.Shift,
                                 applier: Applier)
  extends Applier {
  override def patternVars(): Set[Any] =
    applier.patternVars() - newV + v

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    Analysis.mergeRequired(applier.requiredAnalyses(), (Set(SmallestSizeAnalysis), Set()))

  override def applyOne(egraph: EGraph,
                        eclass: EClassId,
                        shc: Substs)(
                        subst: shc.Subst): Vec[EClassId] = {
    val smallestOf = egraph.getAnalysis(SmallestSizeAnalysis)
    val extract = smallestOf(shc.get(v, subst))._1
    val shifted = extract.shifted(egraph, shift, cutoff)
    val subst2 = shc.insert(newV, egraph.addExpr(shifted), subst)
    applier.applyOne(egraph, eclass, shc)(subst2)
  }
}

/** An [[Applier]] that checks whether a shifted variable is equal to another
  * @note It works by extracting an expression from the [[EGraph]] in order to shift it.
  */
object ShiftedCheckApplier {
  def apply(v: PatternVar, v2: PatternVar,
            shift: Expr.Shift, cutoff: Expr.Shift,
            applier: Applier): Applier =
    new ConditionalApplier(Set(v, v2), (Set(SmallestSizeAnalysis), Set()), applier) {
      override def cond(egraph: EGraph, id: EClassId, substs: Substs)(subst: substs.Subst): Boolean = {
        val smallestOf = egraph.getAnalysis(SmallestSizeAnalysis)
        val extract = smallestOf(substs.get(v, subst))._1
        val shifted = extract.shifted(egraph, shift, cutoff)
        val expected = smallestOf(substs.get(v2, subst))._1
        shifted == expected
      }
    }
}

/** An [[Applier]] that shifts the DeBruijn indices of a nat variable */
case class ShiftedNatApplier(v: NatPatternVar, newV: NatPatternVar,
                                         shift: Nat.Shift, cutoff: Nat.Shift,
                                         applier: Applier)
  extends Applier {
  override def patternVars(): Set[Any] =
    applier.patternVars() - newV + v

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    applier.requiredAnalyses()

  override def applyOne(egraph: EGraph,
                        eclass: EClassId,
                        shc: Substs)(
                        subst: shc.Subst): Vec[EClassId] = {
    val nat = shc.get(v, subst)
    val shifted = NodeSubs.Nat.shifted(egraph, nat, shift, cutoff)
    val subst2 = shc.insert(newV, shifted, subst)
    applier.applyOne(egraph, eclass, shc)(subst2)
  }
}

/** An [[Applier]] that checks whether a shifted nat variable is equal to another */
object ShiftedNatCheckApplier {
  def apply(v: NatPatternVar, v2: NatPatternVar,
            shift: Nat.Shift, cutoff: Nat.Shift,
            applier: Applier): Applier =
    new ConditionalApplier(Set(v, v2), (Set(), Set()), applier) {
      override def cond(egraph: EGraph, id: EClassId, substs: Substs)(subst: substs.Subst): Boolean = {
        val nat = substs.get(v, subst)
        val shifted = NodeSubs.Nat.shifted(egraph, nat, shift, cutoff)
        val expected = substs.get(v2, subst)
        shifted == expected
      }
    }
}

/** An [[Applier]] that shifts the DeBruijn indices of a data type variable */
case class ShiftedDataTypeApplier(v: DataTypePatternVar, newV: DataTypePatternVar,
                                              shift: Type.Shift, cutoff: Type.Shift,
                                              applier: Applier)
  extends Applier {
  override def patternVars(): Set[Any] =
    applier.patternVars() - newV + v

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    applier.requiredAnalyses()

  override def applyOne(egraph: EGraph,
                        eclass: EClassId,
                        shc: Substs)(
                        subst: shc.Subst): Vec[EClassId] = {
    val dt = shc.get(v, subst)
    val shifted = NodeSubs.DataType.shifted(egraph, dt, shift, cutoff)
    val subst2 = shc.insert(newV, shifted, subst)
    applier.applyOne(egraph, eclass, shc)(subst2)
  }
}

/** An [[Applier]] that checks whether a shifted nat variable is equal to another */
object ShiftedDataTypeCheckApplier {
  def apply(v: DataTypePatternVar, v2: DataTypePatternVar,
            shift: Type.Shift, cutoff: Type.Shift,
            applier: Applier): Applier =
    new ConditionalApplier(Set(v, v2), (Set(), Set()), applier) {
      override def cond(egraph: EGraph, id: EClassId, substs: Substs)(subst: substs.Subst): Boolean = {
        val dt = substs.get(v, subst)
        val shifted = NodeSubs.DataType.shifted(egraph, dt, shift, cutoff)
        val expected = substs.get(v2, subst)
        shifted == expected
      }
    }
}

/** An [[Applier]] that shifts the DeBruijn indices of a type variable */
case class ShiftedTypeApplier(v: TypePatternVar, newV: TypePatternVar,
                                          shift: Type.Shift, cutoff: Type.Shift,
                                          applier: Applier)
  extends Applier {
  override def patternVars(): Set[Any] =
    applier.patternVars() - newV + v

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    applier.requiredAnalyses()

  override def applyOne(egraph: EGraph,
                        eclass: EClassId,
                        shc: Substs)(
                        subst: shc.Subst): Vec[EClassId] = {
    val t = shc.get(v, subst)
    val shifted = NodeSubs.Type.shifted(egraph, t, shift, cutoff)
    val subst2 = shc.insert(newV, shifted, subst)
    applier.applyOne(egraph, eclass, shc)(subst2)
  }
}

/** An [[Applier]] that checks whether a shifted nat variable is equal to another */
object ShiftedTypeCheckApplier {
  def apply(v: TypePatternVar, v2: TypePatternVar,
            shift: Type.Shift, cutoff: Type.Shift,
            applier: Applier): Applier =
    new ConditionalApplier(Set(v, v2), (Set(), Set()), applier) {
      override def cond(egraph: EGraph, id: EClassId, substs: Substs)(subst: substs.Subst): Boolean = {
        val t = substs.get(v, subst)
        val shifted = NodeSubs.Type.shifted(egraph, t, shift, cutoff)
        val expected = substs.get(v2, subst)
        shifted == expected
      }
    }
}

/** An [[Applier]] that performs beta-reduction.
  * @note It works by extracting an expression from the [[EGraph]] in order to beta-reduce it.
  */
case class BetaExtractApplier(body: PatternVar, subs: PatternVar)
  extends Applier {
  override def patternVars(): Set[Any] =
    Set(body, subs)

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(BENF.extractAnalysis), Set())

  override def applyOne(egraph: EGraph,
                        eclass: EClassId,
                        shc: Substs)(
                        subst: shc.Subst): Vec[EClassId] = {
    val smallestOf = egraph.getAnalysis(BENF.extractAnalysis)
    val bodyEx = smallestOf(shc.get(body, subst))._1
    val subsEx = smallestOf(shc.get(subs, subst))._1
    val result = bodyEx.withArgument(egraph, subsEx)
    Vec(egraph.addExpr(result))
  }
}

/** An [[Applier]] that performs beta-reduction.
  * @note It works by directly transforming [[EClass]]es.
  */
case class BetaApplier(body: PatternVar, subs: PatternVar)
  extends Applier {
  override def patternVars(): Set[Any] =
    Set(body, subs)

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) = ???

  override def applyOne(egraph: EGraph,
                        eclass: EClassId,
                        shc: Substs)(
                        subst: shc.Subst): Vec[EClassId] = {
    ??? // Vec(EClass.withArgument(subst(body), subst(subs), egraph))
  }
}

/** An [[Applier]] that performs beta-reduction for nat-dependent functions.
  * @note It works by directly transforming [[EClass]]es.
  */
case class BetaNatApplier(body: PatternVar, subs: NatPatternVar)
  extends Applier {
  override def patternVars(): Set[Any] =
    Set(body, subs)

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) = ???

  override def applyOne(egraph: EGraph,
                        eclass: EClassId,
                        shc: Substs)(
                        subst: shc.Subst): Vec[EClassId] = {
    ??? // Vec(EClass.withNatArgument(subst(body), subst(subs), egraph))
  }
}

/** An [[Applier]] that performs beta-reduction for nat-dependent functions.
  * @note It works by extracting an expression from the [[EGraph]] in order to beta-reduce it.
  */
case class BetaNatExtractApplier(body: PatternVar, subs: NatPatternVar)
  extends Applier {
  override def patternVars(): Set[Any] =
    Set(body, subs)

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(BENF.extractAnalysis), Set())

  override def applyOne(egraph: EGraph,
                        eclass: EClassId,
                        shc: Substs)(
                        subst: shc.Subst): Vec[EClassId] = {
    val smallestOf = egraph.getAnalysis(BENF.extractAnalysis)
    val bodyEx = smallestOf(shc.get(body, subst))._1
    val subsNat = shc.get(subs, subst)
    val result = bodyEx.withNatArgument(egraph, subsNat)
    Vec(egraph.addExpr(result))
  }
}

/** An [[Applier]] that checks whether a nat variable is equal to a nat pattern */
object ComputeNatCheckApplier {
  def apply(v: NatPatternVar, expected: NatPattern,
                        applier: Applier): Applier =
    new ConditionalApplier(expected.patternVars() + v, (Set(), Set()), applier) {
      override def cond(egraph: EGraph, id: EClassId, substs: Substs)(subst: substs.Subst): Boolean = {
        // TODO: can we be more efficient here?
        ComputeNat.toNamed(egraph, v, substs)(subst) == ComputeNat.toNamed(egraph, expected, substs)(subst)
      }
    }
}

/** An [[Applier]] that computes a nat variable according to a nat pattern */
case class ComputeNatApplier(v: NatPatternVar, value: NatPattern,
                                         applier: Applier) extends Applier {

  override def patternVars(): Set[Any] =
    applier.patternVars() - v ++ value.patternVars()

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    applier.requiredAnalyses()

  override def applyOne(egraph: EGraph,
                        eclass: EClassId,
                        shc: Substs)(
                        subst: shc.Subst): Vec[EClassId] = {
    // TODO: can we be more efficient here?
    val actualValue = Nat.fromNamedGeneric(
      ComputeNat.toNamed(egraph, value, shc)(subst), ni => ni.name.drop(1).toInt)
    val subst2 = shc.insert(v, egraph.addNat(actualValue), subst)
    applier.applyOne(egraph, eclass, shc)(subst2)
  }
}

private object ComputeNat {
  import arithexpr.arithmetic._

  def toNamed(egraph: EGraph,
              n: NatPattern,
              shc: Substs)(
              subst: shc.Subst): rct.Nat = {
    n match {
      case NatPatternAny => throw new Exception("")
      case pv: NatPatternVar => toNamed(egraph, shc.get(pv, subst))
      case NatPatternNode(node) => node match {
        case NatVar(index) => rct.NatIdentifier(s"n$index")
        case NatCst(value) => Cst(value)
        case NatNegInf => NegInf
        case NatPosInf => PosInf
        case NatAdd(a, b) => toNamed(egraph, a, shc)(subst) + toNamed(egraph, b, shc)(subst)
        case NatMul(a, b) => toNamed(egraph, a, shc)(subst) * toNamed(egraph, b, shc)(subst)
        case NatPow(b, e) => toNamed(egraph, b, shc)(subst).pow(toNamed(egraph, e, shc)(subst))
        case NatMod(a, b) => toNamed(egraph, a, shc)(subst) % toNamed(egraph, b, shc)(subst)
        case NatIntDiv(a, b) => toNamed(egraph, a, shc)(subst) / toNamed(egraph, b, shc)(subst)
      }
    }
  }

  def toNamed(egraph: EGraph, id: NatId): rct.Nat = {
    egraph(id) match {
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

/** An [[Applier]] that vectorizes a scalar function.
  * @note It works by extracting an expression from the [[EGraph]] in order to vectorize it.
  */
case class VectorizeScalarFunExtractApplier(f: PatternVar, n: NatPatternVar, fV: PatternVar,
                                            applier: Applier)
  extends Applier {
  override def patternVars(): Set[Any] = applier.patternVars() - fV

  override def requiredAnalyses(): (Set[Analysis], Set[TypeAnalysis]) =
    (Set(SmallestSizeAnalysis), Set())

  override def applyOne(egraph: EGraph,
                        eclass: EClassId,
                        shc: Substs)(
                        subst: shc.Subst): Vec[EClassId] = {
    val smallestOf = egraph.getAnalysis(SmallestSizeAnalysis)
    val fEx = smallestOf(shc.get(f, subst))._1
    val actualN = shc.get(n, subst)
    vectorizeExpr(fEx, actualN, egraph, Set()) match {
      case Some(fExV) =>
        val subst2 = shc.insert(fV, egraph.addExpr(fExV), subst)
        applier.applyOne(egraph, eclass, shc)(subst2)
      case None => Vec()
    }
  }

  private def vectorizeExpr(expr: ExprWithHashCons,
                            n: NatId,
                            eg: EGraph,
                            vEnv: Set[Int]): Option[ExprWithHashCons] = {
    import rise.core.{primitives => rcp}
    expr.node match {
      case Var(i) if vEnv(i) =>
        for { tv <- vecDT(expr.t, n, eg) }
          yield ExprWithHashCons(Var(i), tv)
      case Var(_) => None
      case App(f, e) =>
        for { fv <- vectorizeExpr(f, n, eg, vEnv); ev <- vectorizeExpr(e, n, eg, vEnv) }
          yield ExprWithHashCons(App(fv, ev), eg(fv.t).asInstanceOf[FunType[TypeId]].outT)
      case Lambda(e) =>
        for { ev <- vectorizeExpr(e, n, eg, vEnv.map(_ + 1) + 0);
              xtv <- vecDT(eg(expr.t).asInstanceOf[FunType[TypeId]].inT, n, eg) }
          yield ExprWithHashCons(Lambda(ev),  eg.add(FunType(xtv, ev.t)))
      case NatApp(_, _) => None
      case DataApp(_, _) => None
      case AddrApp(_, _) => None
      case NatLambda(_) => None
      case DataLambda(_) => None
      case AddrLambda(_) => None
      case Literal(_) =>
        for { tv <- vecDT(expr.t, n, eg) }
          yield ExprWithHashCons(App(
            ExprWithHashCons(Primitive(rcp.vectorFromScalar.primitive), eg.add(FunType(expr.t, tv))),
            expr), tv)
      case Primitive(rcp.add() | rcp.mul() | rcp.fst() | rcp.snd()) =>
        for { tv <- vecT(expr.t, n, eg) }
          yield ExprWithHashCons(expr.node, tv)
      case Primitive(_) => None
      case Composition(f, g) =>
        for { fv <- vectorizeExpr(f, n, eg, vEnv); gv <- vectorizeExpr(g, n, eg, vEnv) }
          yield ExprWithHashCons(Composition(fv, gv), eg.add(FunType(
            eg(fv.t).asInstanceOf[FunType[TypeId]].inT,
            eg(gv.t).asInstanceOf[FunType[TypeId]].outT,
          )))
    }
  }

  private def vecT(t: TypeId, n: NatId, eg: EGraph): Option[TypeId] = {
    t match {
      case dt @ DataTypeId(_) => vecDT(dt, n, eg)
      case ndt @ NotDataTypeId(_) => eg(ndt) match {
        case FunType(inT, outT) =>
          for { inTV <- vecT(inT, n, eg); outTV <- vecT(outT, n, eg) }
            yield eg.add(FunType(inTV, outTV))
        case NatFunType(t) => ???
        case DataFunType(t) => ???
        case AddrFunType(t) => ???
        case _: DataTypeNode[_, _] => throw new Exception("this should not happen")
      }
    }
  }
  private def vecDT(t: TypeId, n: NatId, eg: EGraph): Option[TypeId] = {
    t match {
      case dt @ DataTypeId(_) => vecDT(dt, n, eg)
      case NotDataTypeId(_) => None
    }
  }
  private def vecDT(t: DataTypeId, n: NatId, eg: EGraph): Option[DataTypeId] = {
    eg(t) match {
      case DataTypeVar(_) => None
      case ScalarType(_) => Some(eg.add(VectorType(n, t)))
      case NatType => None
      case VectorType(_, _) => None
      case IndexType(_) => None
      case PairType(a, b) =>
        for { a2 <- vecDT(a, n, eg); b2 <- vecDT(b, n, eg) }
          yield eg.add(PairType(a2, b2))
      case ArrayType(_, _) => None
    }
  }
}