package rise.eqsat

import PatternDSL._
import rise.core.types.{Nat, NatKind}
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

  val mapFusion: Rule = Rewrite.syntactic("map-fusion",
    app(app(map, ?(0)), app(app(map, ?(1)), ?(2))),
    // TODO: inject
    // ShiftedApplier(?(0), _, up = true, 0,
    // ShiftedApplier(?(1), _, up = true, 0,
    app(app(map, lam(app(?(0), app(?(1), %(0))))), ?(2))
  )
  val mapFission: Rule = Rewrite.syntactic("map-fission",
    app(map, lam(app(?(0), ?(1)))),
    // TODO: inject
    // ShiftedApplier(?(1), _, up = true, 1,
    lam(app(app(map, ?(0)), app(app(map, lam(?(1))), %(0))))
  ) when neg(containsIdent(?(0), %(0)))

  // - slide widening -
/* TODO
  val dropInSlide: Rule = Rewrite.init("drop-in-slide",
    app(nApp(drop, ?("l")), app(nApp(nApp(slide, ?("n")), cst(1)), ?("in"))).compile(),
    app(app(map, nApp(drop, ?("l"))), app(nApp(nApp(slide, ?("n") + ?("l")), cst(1)), ?("in")))
  )
  val takeInSlide: Rule = Rewrite.init("take-in-slide",
    app(nApp(take, ?("r")), app(nApp(nApp(slide, ?("n")), cst(1)), ?("in"))).compile(),
    // TODO: match on `take : s._ -> _`
    app(app(map, nApp(take, ?("n"))), app(nApp(nApp(slide, ?("n") + ?("s") - ?("r")), cst(1)), ?("in")))
  )
*/
  // -- reduction --

  val eta: Rule = Rewrite.init("eta",
    lam(app(?(0), %(0))).compile(),
    ConditionalApplier(neg(containsIdent(?(0), %(0))),
      ShiftedApplier(?(0), ?(1), up = false, 1,
        ?(1): Pattern))
  )
  val beta: Rule = Rewrite.init("beta",
    app(lam(?(0)), ?(1)).compile(),
    BetaApplier(?(0), ?(1))
  )
  val removeTransposePair: Rule = Rewrite.syntactic("remove-transpose-pair",
    app(transpose, app(transpose, ?(0))),
    ?(0)
  )

  // -- movement --

  val mapSlideBeforeTranspose: Rule = Rewrite.syntactic("map-slide-before-transpose",
    app(transpose, app(app(map, nApp(nApp(slide, `?n`(0)), `?n`(1))), ?(0))),
    app(app(map, transpose), app(nApp(nApp(slide, `?n`(0)), `?n`(1)), app(transpose, ?(0))))
  )
  val slideBeforeMapMapF: Rule = Rewrite.syntactic("slide-before-map-map-f",
    app(app(map, app(map, ?(0))), app(nApp(nApp(slide, `?n`(0)), `?n`(1)), ?(1))),
    app(nApp(nApp(slide, `?n`(0)), `?n`(1)), app(app(map, ?(0)), ?(1)))
  )
  val slideBeforeMap: Rule = Rewrite.syntactic("slide-before-map",
    app(nApp(nApp(slide, `?n`(0)), `?n`(1)), app(app(map, ?(0)), ?(1))),
    app(app(map, app(map, ?(0))), app(nApp(nApp(slide, `?n`(0)), `?n`(1)), ?(1)))
  )

  val dropBeforeMap: Rule = Rewrite.syntactic("drop-before-map",
    app(nApp(drop, `?n`(0)), app(app(map, ?(0)), ?(1))),
    app(app(map, ?(0)), app(nApp(drop, `?n`(0)), ?(1)))
  )
  val takeBeforeMap: Rule = Rewrite.syntactic("take-before-map",
    app(nApp(take, `?n`(0)), app(app(map, ?(0)), ?(1))),
    app(app(map, ?(0)), app(nApp(take, `?n`(0)), ?(1)))
  )

  // -- lowering --

  val reduceSeqUnroll: Rule = Rewrite.syntactic("reduce-seq-unroll",
    reduce, prim(rcp.reduceSeqUnroll.primitive)
  )
  val mapSeq: Rule = Rewrite.syntactic("map-seq",
    map, prim(rcp.mapSeq.primitive)
  )
  val iterateStream: Rule = Rewrite.syntactic("iterate-stream",
    map, prim(rcp.iterateStream.primitive)
  )
  val toMemAfterMapSeq: Rule = Rewrite.syntactic("to-mem-after-map-seq",
    app(app(prim(rcp.mapSeq.primitive), ?(0)), ?(1)),
    app(prim(rcp.toMem.primitive), app(app(prim(rcp.mapSeq.primitive), ?(0)), ?(1)))
  )
  /* TODO
        rewrite!("rotate-values-simplified";
            "(app (app slide ?sz) 1)" => "(app rotateValues ?sz)"),
  */
}
