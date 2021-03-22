package rise.eqsat

object Rewrite {
  def init[D](name: String, searcher: Searcher[D], applier: Applier[D]): Rewrite[D] = {
    val boundVars = searcher.patternVars()
    for (v <- applier.patternVars()) {
      assert(boundVars.contains(v))
    }

    new Rewrite(name, searcher, applier)
  }
}

class Rewrite[Data](val name: String,
                    val searcher: Searcher[Data],
                    val applier: Applier[Data]) {
  def search(egraph: EGraph[Data]): Vec[SearchMatches] =
    searcher.search(egraph)

  def apply(egraph: EGraph[Data], matches: Vec[SearchMatches]): Vec[EClassId] =
    applier.applyMatches(egraph, matches)
}

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

trait Applier[Data] {
  // the variables used by this applier
  // return an empty Vec to disable checks
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

case class SearchMatches(eclass: EClassId, substs: Vec[Subst])

case class Subst(vec: Vec[(PatternVar, EClassId)]) {
  // insert a mapping, returning the old eclass if present
  def insert(variable: PatternVar, eclass: EClassId): Option[EClassId] = {
    for (((v, ec), i) <- vec.zipWithIndex) {
      if (v == variable) {
        vec.update(i, variable -> eclass)
        return Some(ec)
      }
    }
    vec += variable -> eclass
    None
  }

  def get(variable: PatternVar): Option[EClassId] =
    vec.find(_._1 == variable).map(_._2)

  def apply(variable: PatternVar): EClassId =
    get(variable).get

  def deepClone(): Subst =
    Subst(vec.clone())
}

object Subst {
  def empty: Subst = Subst(Vec.empty)
}

// note: the condition is more general in `egg`
case class ConditionalApplier[D](cond: (EGraph[D], EClassId, Subst) => Boolean,
                                 applier: Applier[D])
  extends Applier[D] {
  override def patternVars(): Vec[PatternVar] =
    applier.patternVars()

  override def applyOne(egraph: EGraph[D], eclass: EClassId, subst: Subst): Vec[EClassId] = {
    if (cond(egraph, eclass, subst)) { applier.applyOne(egraph, eclass, subst) } else { Vec() }
  }
}

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