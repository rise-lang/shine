package rise.eqsat

object Rewrite {
  def init[D](name: String, rule: (Searcher[D], Applier[D])): Rewrite[D] = {
    val (searcher, applier) = rule
    val boundVars = searcher.patternVars()
    for (v <- applier.patternVars()) {
      assert(boundVars.contains(v))
    }

    new Rewrite(name, searcher, applier)
  }

  // this checks and infers types for a purely syntactic rewrite
  def syntactic[D](name: String, rule: (Pattern, Pattern)): Rewrite[D] = {
    import rise.{core => rc}
    import rise.core.{types => rct}
    import arithexpr.{arithmetic => ae}

    // TODO: check that `lhs` and `rhs` have no negative indices for pattern variables to avoid clashes
    var naCount = 0
    var dtaCount = 0

    def exprToBeTyped(pattern: Pattern,
                      bound: Expr.Bound,
                      pvTypes: Map[PatternVar, rct.Type]): rc.Expr = {
      pattern.p match {
        case pv: PatternVar =>
          val tmp = rc.TypeAnnotation(
            rc.Identifier(s"?${pv.index}")(rct.TypeIdentifier(s"?tOfV${pv.index}")),
            typeToBeTyped(pattern.t, bound))
          pvTypes.get(pv) match {
            case None => tmp
            case Some(t) => rc.TypeAssertion(tmp, t)
          }
        case PatternNode(node) => (node match {
          case Var(index) => bound.expr(index).setType _
          case App(f, e) =>
            rc.App(
              exprToBeTyped(f, bound, pvTypes),
              exprToBeTyped(e, bound, pvTypes)) _
          case Lambda(e) =>
            val i = rc.Identifier(s"x${bound.expr.size}")(rct.TypePlaceholder)
            rc.Lambda(i, exprToBeTyped(e, bound + i, pvTypes)) _
          case NatApp(f, x) =>
            rc.DepApp[rct.NatKind](exprToBeTyped(f, bound, pvTypes),
              natToBeTyped(x, bound)) _
          case DataApp(f, x) =>
            rc.DepApp[rct.DataKind](exprToBeTyped(f, bound, pvTypes),
              dataToBeTyped(x, bound)) _
          case NatLambda(e) =>
            val i = rct.NatIdentifier(s"n${bound.nat.size}", isExplicit = true)
            rc.DepLambda[rct.NatKind](i, exprToBeTyped(e, bound, pvTypes)) _
          case DataLambda(e) =>
            val i = rct.DataTypeIdentifier(s"dt${bound.nat.size}", isExplicit = true)
            rc.DepLambda[rct.DataKind](i, exprToBeTyped(e, bound, pvTypes)) _
          case Literal(d) => rc.Literal(d).setType _
          case Primitive(p) => p.setType _
        })(typeToBeTyped(pattern.t, bound))
      }
    }
    def natToBeTyped(pattern: NatPattern, bound: Expr.Bound): rct.Nat = {
      pattern match {
        case pv: NatPatternVar =>
          rct.NatIdentifier(s"?n${pv.index}", isExplicit = true)
        case NatPatternAny => // "NatPlaceholder"
          val ident = rct.NatIdentifier(s"?n_$naCount", isExplicit = false)
          naCount += 1
          ident
        case NatPatternNode(node) => node match {
          case NatVar(index) => bound.nat(index)
          case NatCst(value) => value: rct.Nat
          case NatAdd(a, b) => natToBeTyped(a, bound) + natToBeTyped(b, bound)
          case NatMul(a, b) => natToBeTyped(a, bound) * natToBeTyped(b, bound)
        }
      }
    }
    def dataToBeTyped(pattern: DataTypePattern, bound: Expr.Bound): rct.DataType = {
      pattern match {
        case pv: DataTypePatternVar =>
          rct.DataTypeIdentifier(s"?dt${pv.index}", isExplicit = true)
        case DataTypePatternAny => // "DataTypePlaceholder"
          val ident = rct.DataTypeIdentifier(s"?dt_$dtaCount", isExplicit = false)
          dtaCount += 1
          ident
        case DataTypePatternNode(node) => node match {
          case DataTypeVar(index) => bound.data(index)
          case ScalarType(s) => s
          case NatType => rct.NatType
          case VectorType(size, elemType) =>
            rct.VectorType(natToBeTyped(size, bound), dataToBeTyped(elemType, bound))
          case IndexType(size) =>
            rct.IndexType(natToBeTyped(size, bound))
          case PairType(dt1, dt2) =>
            rct.PairType(dataToBeTyped(dt1, bound), dataToBeTyped(dt2, bound))
          case ArrayType(size, elemType) =>
            rct.ArrayType(natToBeTyped(size, bound), dataToBeTyped(elemType, bound))
        }
      }
    }
    def typeToBeTyped(pattern: TypePattern, bound: Expr.Bound): rct.Type = {
      pattern match {
        case pv: TypePatternVar =>
          throw new Exception("no support for type pattern variables here")
          // rct.TypeIdentifier(s"?t${pv.index}", isExplicit = true)
        case TypePatternAny => rct.TypePlaceholder
        case dtp: DataTypePattern => dataToBeTyped(dtp, bound)
        case TypePatternNode(n) => n match {
          case FunType(inT, outT) =>
            rct.FunType(typeToBeTyped(inT, bound), typeToBeTyped(outT, bound))
          case NatFunType(t) =>
            val i = rct.NatIdentifier(s"n${bound.nat.size}", isExplicit = true)
            rct.DepFunType[rct.NatKind, rct.Type](i, typeToBeTyped(t, bound + i))
          case DataFunType(t) =>
            val i = rct.DataTypeIdentifier(s"dt${bound.data.size}", isExplicit = true)
            rct.DepFunType[rct.DataKind, rct.Type](i, typeToBeTyped(t, bound + i))
          case dtn: DataTypeNode[NatPattern, DataTypePattern] =>
            dataToBeTyped(DataTypePatternNode(dtn), bound)
        }
      }
    }

    def collectIdentifierTypes(e: rc.Expr): Map[PatternVar, rct.Type] = {
      val traversal = new rc.traverse.PureAccumulatorTraversal[Map[PatternVar, rct.Type]] {
        override val accumulator = util.monads.MapMonoid[PatternVar, rct.Type]
        override def expr: rc.Expr => Pair[rc.Expr] = {
          case i @ rc.Identifier(name) if name.startsWith("?") =>
            val pv = PatternVar(name.drop(1).toInt)
            // TODO: check that i.t contains no local variable, or deal with it?
            accumulate(Map(pv -> i.t))(i: rc.Expr)
          case e => super.expr(e)
        }
      }
      rc.traverse.traverse(e, traversal)._1
    }

    // TODO: for every lhs pattern variable, check its Bound context,
    //  deal with multiple occurences in different contexts somehow,
    //  for every rhs pattern variable, shift it according to its new context
    val lhsPVBound = HashMap[PatternVar, Expr.Bound]()
    val lhsNPVBound = HashMap[NatPatternVar, Expr.Bound]()
    val lhsDTPVBound = HashMap[DataTypePatternVar, Expr.Bound]()
    val lhsTPVBound = HashMap[TypePatternVar, Expr.Bound]()

    def checkPVBound[K](pv: K, m: HashMap[K, Expr.Bound], bound: Expr.Bound, isRhs: Boolean): Unit = {
      if (!isRhs) {
        val b = m.getOrElseUpdate(pv, bound)
        assert( // FIXME: find a way to match over shifted pattern variables
          b.expr.size == bound.expr.size &&
            b.nat.size == bound.nat.size &&
            b.data.size == bound.data.size
        )
      } else {
        val b = m.getOrElse(pv,
          throw new Exception(s"$pv was not bound on the left-hand-side"))
      }
    }

    val typesToMatchFor = HashMap[rct.TypeIdentifier, TypePatternVar]()
    val dataTypesToMatchFor = HashMap[rct.DataTypeIdentifier, DataTypePatternVar]()
    val natsToMatchFor = HashMap[rct.NatIdentifier, NatPatternVar]()
    val matchedTypes = HashSet[TypePatternVar]()
    val matchedDataTypes = HashSet[DataTypePatternVar]()
    val matchedNats = HashSet[NatPatternVar]()

    def typedPattern(expr: rc.Expr, bound: Expr.Bound, isRhs: Boolean): Pattern = {
      Pattern(expr match {
        case i: rc.Identifier if i.name.startsWith("?") => PatternVar(i.name.drop(1).toInt)
        case i: rc.Identifier => PatternNode(Var(bound.indexOf(i)))
        case rc.Lambda(x, e) =>
          PatternNode(Lambda(typedPattern(e, bound + x, isRhs)))
        case rc.App(f, e) =>
          PatternNode(App(typedPattern(f, bound, isRhs), typedPattern(e, bound, isRhs)))
        case rc.DepLambda(x: rct.NatIdentifier, e) =>
          PatternNode(NatLambda(typedPattern(e, bound + x, isRhs)))
        case rc.DepLambda(x: rct.DataTypeIdentifier, e) =>
          PatternNode(DataLambda(typedPattern(e, bound + x, isRhs)))
        case rc.DepLambda(_, _) => ???
        case rc.DepApp(f, x: rct.Nat) =>
          PatternNode(NatApp(typedPattern(f, bound, isRhs), typedNat(x, bound, isRhs)))
        case rc.DepApp(f, x: rct.DataType) =>
          PatternNode(DataApp(typedPattern(f, bound, isRhs), typedData(x, bound, isRhs)))
        case rc.DepApp(_, _) => ???
        case rc.Literal(d) => PatternNode(Literal(d))
        case p: rc.Primitive => PatternNode(Primitive(p))
      }, typedType(expr.t, bound, isRhs))
    }
    def typedNat(n: rct.Nat, bound: Expr.Bound, isRhs: Boolean): NatPattern = {
      n match {
        case i: rct.NatIdentifier if !i.isExplicit =>
          // note: negative variable index to avoid clash with user
          val pv = natsToMatchFor.getOrElseUpdate(i, NatPatternVar(- (1 + natsToMatchFor.size)))
          if (!isRhs) { matchedNats.add(pv) }
          pv
        case i: rct.NatIdentifier if i.name.startsWith("?n") =>
          NatPatternVar(i.name.drop(2).toInt)
        case i: rct.NatIdentifier =>
          NatPatternNode(NatVar(bound.indexOf(i)))
        case ae.Cst(c) =>
          NatPatternNode(NatCst(c))
        case ae.Sum(Nil) => NatPatternNode(NatCst(0))
        case ae.Sum(t +: ts) => ts.foldRight(typedNat(t, bound, isRhs)) { case (t, acc) =>
          NatPatternNode(NatAdd(typedNat(t, bound, isRhs), acc))
        }
        case ae.Prod(Nil) => NatPatternNode(NatCst(1))
        case ae.Prod(t +: ts) => ts.foldRight(typedNat(t, bound, isRhs)) { case (t, acc) =>
          NatPatternNode(NatMul(typedNat(t, bound, isRhs), acc))
        }
        case _ => throw new Exception(s"did not expect $n")
      }
    }
    def typedData(dt: rct.DataType, bound: Expr.Bound, isRhs: Boolean): DataTypePattern = {
      dt match {
        case i: rct.DataTypeIdentifier if !i.isExplicit =>
          // note: negative variable index to avoid clash with user
          val pv = dataTypesToMatchFor.getOrElseUpdate(i, DataTypePatternVar(- (1 + dataTypesToMatchFor.size)))
          if (!isRhs) { matchedDataTypes.add(pv) }
          pv
        case i: rct.DataTypeIdentifier if i.name.startsWith("?dt") =>
          DataTypePatternVar(i.name.drop(3).toInt)
        case i: rct.DataTypeIdentifier =>
          DataTypePatternNode(DataTypeVar(bound.indexOf(i)))
        case rct.NatType =>
          DataTypePatternNode(NatType)
        case rct.VectorType(s, et) =>
          DataTypePatternNode(VectorType(typedNat(s, bound, isRhs), typedData(et, bound, isRhs)))
        case rct.IndexType(s) =>
          DataTypePatternNode(IndexType(typedNat(s, bound, isRhs)))
        case rct.PairType(dt1, dt2) =>
          DataTypePatternNode(PairType(typedData(dt1, bound, isRhs), typedData(dt2, bound, isRhs)))
        case rct.ArrayType(s, et) =>
          DataTypePatternNode(ArrayType(typedNat(s, bound, isRhs), typedData(et, bound, isRhs)))
        case _: rct.DepArrayType | _: rct.DepPairType[_] |
             _: rct.NatToDataApply | _: rct.FragmentType =>
          throw new Exception(s"did not expect $dt")
      }
    }
    def typedType(t: rct.Type, bound: Expr.Bound, isRhs: Boolean): TypePattern = {
      t match {
        case dt: rct.DataType => typedData(dt, bound, isRhs)
        case rct.FunType(a, b) =>
          TypePatternNode(FunType(typedType(a, bound, isRhs), typedType(b, bound, isRhs)))
        case rct.DepFunType(x: rct.NatIdentifier, t) =>
          TypePatternNode(NatFunType(typedType(t, bound + x, isRhs)))
        case rct.DepFunType(x: rct.DataTypeIdentifier, t) =>
          TypePatternNode(DataFunType(typedType(t, bound + x, isRhs)))
        case rct.DepFunType(_, _) => ???
        case i: rct.TypeIdentifier if isRhs =>
          // note: negative variable index to avoid clash with user
          val pv = typesToMatchFor.getOrElseUpdate(i, TypePatternVar(- (1 + typesToMatchFor.size)))
          if (!isRhs) { matchedTypes.add(pv) }
          pv
        case rct.TypePlaceholder =>
          throw new Exception(s"did not expect $t, something was not infered")
      }
    }

    import rc.DSL.TypeAssertionHelper

    val (lhs, rhs) = rule
    // whatever the type of the left-hand-side is
    val typedLhsNamed: rc.Expr = rc.DSL.toBeTyped(
      exprToBeTyped(lhs, Expr.Bound.empty, Map()))
    // .. the type of the right-hand-side must be the same
    // .. and the type of the pattern variables must be the same
    val pvTypes = collectIdentifierTypes(typedLhsNamed)
    val typedRhsNamed: rc.Expr = rc.DSL.toBeTyped(
      exprToBeTyped(rhs, Expr.Bound.empty, pvTypes)) !: typedLhsNamed.t
    val typedRhs = typedPattern(typedRhsNamed, Expr.Bound.empty, isRhs = true)
    // .. and we might need to match on some left-hand-side types to construct the typed right-hand-side
    // FIXME:
    //  (1) this matches over more types than necessary and could be optimized
    //  (2) this also throws away type patterns from the original left-hand-side pattern
    //  (3) matching nats is most likely brittle
    val typedLhs = typedPattern(typedLhsNamed, Expr.Bound.empty, isRhs = false)
    val implicitsAreBound =
      typesToMatchFor.size == matchedTypes.size &&
      natsToMatchFor.size == matchedNats.size &&
      dataTypesToMatchFor.size == matchedDataTypes.size
    assert(implicitsAreBound)
    Rewrite.init(name,
      CompiledPattern.patternToSearcher(typedLhs.compile()) ->
      Pattern.patternToApplier(typedRhs))
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
                          up: Boolean, cutoff: Int,
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
    val shifted = extract.shifted(up, cutoff)
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
