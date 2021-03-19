package rise.eqsat

import PatternDSL._
import rise.core.{primitives => rcp}

object rules {
  type Rule = Rewrite[DefaultAnalysisData]

  def containsIdent(v: PatternVar, ident: Var)
                   (egraph: EGraph[DefaultAnalysisData], eclass: EClassId, subst: Subst): Boolean =
    egraph.getMut(subst(v)).data.free.contains(ident.index)

  def neg[D](cond: (EGraph[D], EClassId, Subst) => Boolean)
            (egraph: EGraph[D], eclass: EClassId, subst: Subst): Boolean =
    !cond(egraph, eclass, subst)

  // TODO: automate DeBruijn indices shifting
  // TODO: find a way to combine different analysis requirements?

  // -- algorithmic --

  val mapFusion: Rule = Rewrite.init("map-fusion",
    app(app(map, ?("f")), app(app(map, ?("g")), ?("arg"))).compile(),
    ShiftedApplier(?("f"), ?("fu"), up = true, 0,
      ShiftedApplier(?("g"), ?("gu"), up = true, 0,
        app(app(map, lam(app(?("fu"), app(?("gu"), %(0))))), ?("arg"))))
  )
  val mapFission: Rule = Rewrite.init("map-fission",
    app(map, lam(app(?("f"), ?("gx")))).compile(),
    ShiftedApplier(?("gx"), ?("gxu"), up = true, 1,
        lam(app(app(map, ?("f")), app(app(map, lam(?("gxu"))), %(0)))))
  )

  // -- reduction --

  val eta: Rule = Rewrite.init("eta",
    lam(app(?("f"), %(0))).compile(),
    ConditionalApplier(neg(containsIdent(?("f"), %(0))),
      ShiftedApplier(?("f"), ?("fd"), up = false, 1,
        ?("fd"): Pattern))
  )
  val beta: Rule = Rewrite.init("beta",
    app(lam(?("body")), ?("e")).compile(),
    BetaApplier(?("body"), ?("e"))
  )
  val removeTransposePair: Rule = Rewrite.init("remove-transpose-pair",
    app(transpose, app(transpose, ?("x"))).compile(),
    ?("x"): Pattern
  )

  // -- movement --

  // FIXME: rewrite depApp
  val mapSlideBeforeTranspose: Rule = Rewrite.init("map-slide-before-transpose",
    app(transpose, app(app(map, app(app(slide, ?("sz")), ?("sp"))), ?("y"))).compile(),
    app(app(map, transpose), app(app(app(slide, ?("sz")), ?("sp")), app(transpose, ?("y"))))
  )
  val slideBeforeMapMapF: Rule = Rewrite.init("slide-before-map-map-f",
    app(app(map, app(map, ?("f"))), app(app(app(slide, ?("sz")), ?("sp")), ?("y"))).compile(),
    app(app(app(slide, ?("sz")), ?("sp")), app(app(map, ?("f")), ?("y")))
  )
  val slideBeforeMap: Rule = Rewrite.init("slide-before-map",
    app(app(app(slide, ?("sz")), ?("sp")), app(app(map, ?("f")), ?("y"))).compile(),
    app(app(map, app(map, ?("f"))), app(app(app(slide, ?("sz")), ?("sp")), ?("y")))
  )

  // -- lowering --

  val reduceSeqUnroll: Rule = Rewrite.init("reduce-seq-unroll",
    reduce.compile(), prim(rcp.reduceSeqUnroll.primitive)
  )
  val mapSeq: Rule = Rewrite.init("map-seq",
    map.compile(), prim(rcp.mapSeq.primitive)
  )
  val iterateStream: Rule = Rewrite.init("iterate-stream",
    map.compile(), prim(rcp.iterateStream.primitive)
  )
  val toMemAfterMapSeq: Rule = Rewrite.init("to-mem-after-map-seq",
    app(app(prim(rcp.mapSeq.primitive), ?("f")), ?("x")).compile(),
    app(prim(rcp.toMem.primitive), app(app(prim(rcp.mapSeq.primitive), ?("f")), ?("x")))
  )
  /* TODO
        rewrite!("rotate-values-simplified";
            "(app (app slide ?sz) 1)" => "(app rotateValues ?sz)"),
  */
}
