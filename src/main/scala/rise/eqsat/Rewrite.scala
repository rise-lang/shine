package rise.eqsat

import rise.core.{DepApp, DepLambda, Identifier}
import rise.{core, eqsat}

object Rewrite {
  def init[D](name: String, searcher: Searcher[D], applier: Applier[D]): Rewrite[D] = {
    val boundVars = searcher.patternVars()
    for (v <- applier.patternVars()) {
      assert(boundVars.contains(v))
    }

    new Rewrite(name, searcher, applier)
  }

  // this checks and infers types for a purely syntactic rewrite
  def syntactic[D](name: String, lhs: Pattern, rhs: Pattern): Rewrite[D] = {
    import rise.{core => rc}
    import rise.core.{types => rct}

    var naCount = 0
    var dtaCount = 0

    def exprToBeTyped(rhs: Pattern, bound: Expr.Bound): rc.Expr = {
      rhs.p match { // TODO? could use DSL building ToBeTyped things
        case pv: PatternVar =>
          rc.Identifier(s"?${pv.index}")(typeToBeTyped(rhs.t, bound))
        case PatternNode(node) => (node match {
          case Var(index) => bound.expr(index).setType _
          case App(f, e) =>
            rc.App(exprToBeTyped(f, bound), exprToBeTyped(e, bound)) _
          case Lambda(e) =>
            val i = rc.Identifier(s"x${bound.expr.size}")(rct.TypePlaceholder)
            rc.Lambda(i, exprToBeTyped(e, bound + i)) _
          case NatApp(f, x) =>
            rc.DepApp[rct.NatKind](exprToBeTyped(f, bound), natToBeTyped(x, bound)) _
          case DataApp(f, x) =>
            rc.DepApp[rct.DataKind](exprToBeTyped(f, bound), dataToBeTyped(x, bound)) _
          case NatLambda(e) =>
            val i = rct.NatIdentifier(s"n${bound.nat.size}", isExplicit = true)
            rc.DepLambda[rct.NatKind](i, exprToBeTyped(e, bound)) _
          case DataLambda(e) =>
            val i = rct.DataTypeIdentifier(s"dt${bound.nat.size}", isExplicit = true)
            rc.DepLambda[rct.DataKind](i, exprToBeTyped(e, bound)) _
          case eqsat.Literal(d) => rc.Literal(d).setType _
          case eqsat.Primitive(p) => p.setType _
        })(typeToBeTyped(rhs.t, bound))
      }
    }
    def natToBeTyped(rhs: NatPattern, bound: Expr.Bound): rct.Nat = {
      rhs match {
        case pv: NatPatternVar =>
          rct.NatIdentifier(s"?n${pv.index}", isExplicit = true)
        case NatPatternAny => // "NatPlaceholder"
          val ident = rct.NatIdentifier(s"?n_$naCount", isExplicit = false)
          naCount += 1
          ident
        case NatPatternNode(node) => node match {
          case NatVar(index) => bound.nat(index)
          case NatCst(value) => value: rct.Nat
        }
      }
    }
    def dataToBeTyped(rhs: DataTypePattern, bound: Expr.Bound): rct.DataType = {
      rhs match {
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
    def typeToBeTyped(rhs: TypePattern, bound: Expr.Bound): rct.Type = {
      rhs match {
        // case pv: TypePatternVar =>
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

    def typedPattern(expr: rc.Expr, bound: Expr.Bound): Pattern = {
      Pattern(expr match {
        case i: rc.Identifier if i.name.startsWith("?") => PatternVar(i.name.drop(1).toInt)
        case i: rc.Identifier => PatternNode(Var(bound.indexOf(i)))
        case rc.Lambda(x, e) =>
          PatternNode(Lambda(typedPattern(e, bound + x)))
        case rc.App(f, e) =>
          PatternNode(App(typedPattern(f, bound), typedPattern(e, bound)))
        case rc.DepLambda(x: rct.NatIdentifier, e) =>
          PatternNode(NatLambda(typedPattern(e, bound + x)))
        case rc.DepLambda(x: rct.DataTypeIdentifier, e) =>
          PatternNode(DataLambda(typedPattern(e, bound + x)))
        case rc.DepLambda(_, _) => ???
        case rc.DepApp(f, x: rct.Nat) =>
          PatternNode(NatApp(typedPattern(f, bound), typedNat(x, bound)))
        case rc.DepApp(f, x: rct.DataType) =>
          PatternNode(DataApp(typedPattern(f, bound), typedData(x, bound)))
        case rc.DepApp(_, _) => ???
        case rc.Literal(d) => PatternNode(Literal(d))
        case p: rc.Primitive => PatternNode(Primitive(p))
      }, typedType(expr.t, bound))
    }
    def typedNat(n: rct.Nat, bound: Expr.Bound): NatPattern = {
      n match {
        case i: rct.NatIdentifier if i.name.startsWith("?n_") =>
          throw new Exception("nat could not be inferred")
        case i: rct.NatIdentifier if i.name.startsWith("?n") =>
          NatPatternVar(i.name.drop(2).toInt)
        case i: rct.NatIdentifier =>
          NatPatternNode(NatVar(bound.indexOf(i)))
        case arithexpr.arithmetic.Cst(c) =>
          NatPatternNode(NatCst(c))
        case _ => ???
      }
    }
    def typedData(dt: rct.DataType, bound: Expr.Bound): DataTypePattern = {
      dt match {
        case i: rct.DataTypeIdentifier if i.name.startsWith("?dt_") =>
          throw new Exception("data type could not be inferred")
        case i: rct.DataTypeIdentifier if i.name.startsWith("?dt") =>
          DataTypePatternVar(i.name.drop(3).toInt)
        case i: rct.DataTypeIdentifier =>
          DataTypePatternNode(DataTypeVar(bound.indexOf(i)))
        case rct.NatType =>
          DataTypePatternNode(NatType)
        case rct.VectorType(s, et) =>
          DataTypePatternNode(VectorType(typedNat(s, bound), typedData(et, bound)))
        case rct.IndexType(s) =>
          DataTypePatternNode(IndexType(typedNat(s, bound)))
        case rct.PairType(dt1, dt2) =>
          DataTypePatternNode(PairType(typedData(dt1, bound), typedData(dt2, bound)))
        case rct.ArrayType(s, et) =>
          DataTypePatternNode(ArrayType(typedNat(s, bound), typedData(et, bound)))
        case _: rct.DepArrayType | _: rct.DepPairType[_] |
             _: rct.NatToDataApply | _: rct.FragmentType =>
          throw new Exception(s"did not expect $dt")
      }
    }
    def typedType(t: rct.Type, bound: Expr.Bound): TypePattern = {
      t match {
        case dt: rct.DataType => typedData(dt, bound)
        case rct.FunType(a, b) =>
          TypePatternNode(FunType(typedType(a, bound), typedType(b, bound)))
        case rct.DepFunType(x: rct.NatIdentifier, t) =>
          TypePatternNode(NatFunType(typedType(t, bound + x)))
        case rct.DepFunType(x: rct.DataTypeIdentifier, t) =>
          TypePatternNode(DataFunType(typedType(t, bound + x)))
        case rct.DepFunType(_, _) => ???
        case rct.TypePlaceholder | rct.TypeIdentifier(_) =>
          throw new Exception(s"did not expect $t, something was not infered")
      }
    }

    val typed: rc.Expr = rc.DSL.toBeTyped(exprToBeTyped(rhs, Expr.Bound.empty))
    val typedRhs = typedPattern(typed, Expr.Bound.empty)
    Rewrite.init(name, lhs.compile(), typedRhs)
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
                 //types: VecMap[TypePatternVar, Type],
                 datatypes: VecMap[DataTypePatternVar, DataType]) {
  def insert(pv: PatternVar, eclass: EClassId): Option[EClassId] =
    exprs.insert(pv, eclass)
  def insert(nv: NatPatternVar, n: Nat): Option[Nat] =
    nats.insert(nv, n)
  //def insert(tv: TypePatternVar, t: Type): Option[Type] =
  //  types.insert(tv, t)
  def insert(dtv: DataTypePatternVar, dt: DataType): Option[DataType] =
    datatypes.insert(dtv, dt)

  def apply(pv: PatternVar): EClassId =
    exprs(pv)
  def apply(nv: NatPatternVar): Nat =
    nats(nv)
  //def apply(tv: TypePatternVar): Type =
  //  types(tv)
  def apply(dtv: DataTypePatternVar): DataType =
    datatypes(dtv)

  def deepClone(): Subst =
    Subst(exprs.shallowClone(), nats.shallowClone(), /*types.shallowClone(), */datatypes.shallowClone())
}

object Subst {
  def empty: Subst = Subst(VecMap.empty, VecMap.empty, /*VecMap.empty, */VecMap.empty)
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
