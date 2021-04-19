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

  // -- reduction --

  val eta: Rule = Rewrite.init("eta",
    lam(app(?(0), %(0))).compile()
      -->
      ConditionalApplier(neg(containsIdent(?(0), %(0))),
        ShiftedApplier(?(0), ?(1), -1, 1,
          ?(1): Pattern))
  )
  val beta: Rule = Rewrite.init("beta",
    app(lam(?(0)), ?(1)).compile()
      -->
      BetaApplier(?(0), ?(1))
  )

  import rise.core.types.{Nat, DataType, Type}
  import NamedRewriteDSL._

  val removeTransposePair: Rule = NamedRewrite.init("remove-transpose-pair",
    app(transpose, app(transpose, "x")) --> "x"
  )

  // -- algorithmic --

  // ?(0) + \. ?(0) + %(0) --> ?(0)
  // x + \y. z + y
  // \x. x + \y. x + y --> \x. x
  // \x. %(0) + \. %(1) + %(0)
  // \. %(0) + \. %(1) + %(0)
  // in e-graph: %(0) != %(1)

  lazy val mapFusion: Rule = NamedRewrite.init("map-fusion",
    app(app(map, "f"), app(app(map, "g"), "in"))
      -->
    // TODO: inject
    // ShiftedApplier(?(0), _, up = true, 0,
    // ShiftedApplier(?(1), _, up = true, 0,
    app(app(map, lam("x", app("f", app("g", "x")))), "in")
  )
  val mapFission: Rule = NamedRewrite.init("map-fission",
    app(map, lam("x", app("f", "gx" :: ("dt": DataType))))
      -->
    // TODO: inject
    // ShiftedApplier(?(1), _, up = true, 1,
    lam("in", app(app(map, "f"), app(app(map, lam("x", "gx")), "in")))
  ) when neg(containsIdent(?(0), %(0)))

  // - slide widening -

  // TODO:
  lazy val dropInSlide: Rule = NamedRewrite.init("drop-in-slide",
    app(nApp(drop, "l"), app(nApp(nApp(slide, "n"), 1: Nat), "in"))
      -->
    app(app(map, nApp(drop, "l")), app(nApp(nApp(slide, ("n": Nat) + ("l": Nat)), 1: Nat), "in"))
  )
  lazy val takeInSlide: Rule = NamedRewrite.init("take-in-slide",
    app(nApp(take, "r") :: ((("s": Nat)`.``_`) ->: `_`), app(nApp(nApp(slide, "n"), 1: Nat), "in"))
      -->
    app(app(map, nApp(take, "n")), app(nApp(nApp(slide, ("n": Nat) + ("s": Nat) - ("r": Nat)), 1: Nat), "in"))
  )

  // -- movement --

  // FIXME: matching shifted nats
  lazy val mapSlideBeforeTranspose: Rule = NamedRewrite.init("map-slide-before-transpose",
    app(transpose, app(app(map, nApp(nApp(slide, "sz"), "sp")), "in"))
      -->
    app(app(map, transpose), app(nApp(nApp(slide, "sz"), "sp"), app(transpose, "in")))
  )
  lazy val slideBeforeMapMapF: Rule = NamedRewrite.init("slide-before-map-map-f",
    app(app(map, app(map, "f")), app(nApp(nApp(slide, "sz"), "sp"), "in"))
      -->
    app(nApp(nApp(slide, "sz"), "sp"), app(app(map, "f"), "in"))
  )
  lazy val slideBeforeMap: Rule = NamedRewrite.init("slide-before-map",
    app(nApp(nApp(slide, "sz"), "sp"), app(app(map, "f"), "in"))
      -->
    app(app(map, app(map, "f")), app(nApp(nApp(slide, "sz"), "sp"), "in"))
  )

  // FIXME: matching shifted nats
  lazy val dropBeforeMap: Rule = NamedRewrite.init("drop-before-map",
    app(nApp(drop, "n"), app(app(map, "f"), "in"))
      -->
    app(app(map, "f"), app(nApp(drop, "n"), "in"))
  )
  lazy val takeBeforeMap: Rule = NamedRewrite.init("take-before-map",
    app(nApp(take, "n"), app(app(map, "f"), "in"))
      -->
    app(app(map, "f"), app(nApp(take, "n"), "in"))
  )

  // -- lowering --

  val reduceSeqUnroll: Rule = NamedRewrite.init("reduce-seq-unroll",
    reduce --> rcp.reduceSeqUnroll.primitive
  )
  val mapSeq: Rule = NamedRewrite.init("map-seq",
    map --> rcp.mapSeq.primitive
  )
  val iterateStream: Rule = NamedRewrite.init("iterate-stream",
    map --> rcp.iterateStream.primitive
  )
  val toMemAfterMapSeq: Rule = NamedRewrite.init("to-mem-after-map-seq",
    app(app(rcp.mapSeq.primitive, "f"), "in")
      -->
    app(rcp.toMem.primitive, app(app(rcp.mapSeq.primitive, "f"), "in"))
  )
  /* TODO
        rewrite!("rotate-values-simplified";
            "(app (app slide ?sz) 1)" => "(app rotateValues ?sz)"),
  */
}
