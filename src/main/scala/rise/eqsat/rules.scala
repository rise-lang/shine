package rise.eqsat

import PatternDSL._

object rules {
  type Rule = Rewrite[DefaultAnalysisData]

  def containsIdent(v: PatternVar, ident: Var)
                   (egraph: EGraph[DefaultAnalysisData], eclass: EClassId, subst: Subst): Boolean =
    egraph(subst(v)).data.free.contains(ident.index)

  def neg[D](cond: (EGraph[D], EClassId, Subst) => Boolean)
            (egraph: EGraph[D], eclass: EClassId, subst: Subst): Boolean =
    !cond(egraph, eclass, subst)

  // TODO: automate DeBruijn indices shifting

  val mapFusion: Rule = Rewrite.init("map-fusion",
    app(app(map, ?("f")), app(app(map, ?("g")), ?("arg"))).compile(),
    ShiftedApplier(?("f"), ?("fu"), true, 0,
      ShiftedApplier(?("g"), ?("gu"), true, 0,
        app(app(map, lam(app(?("fu"), app(?("gu"), %(0))))), ?("arg")).compile()))
  )
  val mapFission: Rule = Rewrite.init("map-fission",
    app(map, lam(app(?("f"), ?("gx")))).compile(),
    ShiftedApplier(?("gx"), ?("gxu"), true, 1,
        lam(app(app(map, ?("f")), app(app(map, lam(?("gxu"))), %(0)))).compile())
  )

  val eta: Rule = Rewrite.init("eta",
    lam(app(?("f"), %(0))).compile(),
    ConditionalApplier(neg(containsIdent(?("f"), %(0))),
      ShiftedApplier(?("f"), ?("fd"), false, 1,
        ?("fd").compile()))
  )
  val beta: Rule = Rewrite.init("beta",
    app(lam(?("body")), ?("e")).compile(),
    BetaApplier(?("body"), ?("e"))
  )
}
