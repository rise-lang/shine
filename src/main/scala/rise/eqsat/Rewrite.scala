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

class Rewrite[Data](name: String,
                    searcher: Searcher[Data],
                    applier: Applier[Data]) {
  def search(egraph: EGraph[Data]): Vec[SearchMatches] =
    searcher.search(egraph)

  def apply(egraph: EGraph[Data], matches: Vec[SearchMatches]): Vec[EClassId] =
    applier.apply(egraph, matches)
}

trait Searcher[Data] {
  // the variables bound by this searcher
  def patternVars(): Vec[PatternVar]

  // search one eclass, returning None if no matches can be found
  def searchOne(egraph: EGraph[Data], eclass: EClassId): Option[SearchMatches]

  // search the whole egraph, returning all matches
  def search(egraph: EGraph[Data]): Vec[SearchMatches] = {
    egraph.classes.flatMap(e => searchOne(egraph, e._1)).to(Vec)
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

  def apply(egraph: EGraph[Data], matches: Vec[SearchMatches]): Vec[EClassId] = {
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
        vec(i) = variable -> eclass
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
}

object Subst {
  def empty: Subst = Subst(Vec.empty)
}