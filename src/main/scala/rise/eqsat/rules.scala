package rise.eqsat

import PatternDSL._
import rise.core.{primitives => rcp}
import rise.eqsat.NamedRewriteDSL.`_`

object rules {

  def notContainsIdent(v: PatternVar, ident: Var, freeAnalysis: FreeAnalysisCustomisable)
                   (egraph: EGraph,
                    eclass: EClassId,
                    shc: SubstHashCons,
                    subst: Subst): Boolean = {
    val freeOf = egraph.getAnalysis(freeAnalysis)
    !freeOf(subst(v, shc)).free.contains(ident.index)
  }

  // TODO: find a way to combine different analysis requirements?

  // -- reduction --

  val etaWithFreeIntersection = Rewrite.init("eta-wfi",
    lam(app(?(0), %(0))).compile()
      -->
    ConditionalApplier(
      notContainsIdent(?(0), %(0), FreeIntersectionAnalysis),
      Set(?(0)),
      (Set(FreeIntersectionAnalysis), Set()),
      ShiftedExtractApplier(?(0), ?(1), (-1, 0, 0), (1, 0, 0), ?(1): Pattern))
  )

  val beta = Rewrite.init("beta",
    app(lam(?(0)), ?(1)).compile()
      -->
    BetaApplier(?(0), ?(1))
  )
  val betaNat = Rewrite.init("beta-nat",
    nApp(nLam(?(0)), `?n`(0)).compile()
      -->
    BetaNatApplier(?(0), `?n`(0))
  )
  val betaExtract = Rewrite.init("beta-extract",
    app(lam(?(0)), ?(1)).compile()
      -->
    BetaExtractApplier(?(0), ?(1))
  )
  val betaNatExtract = Rewrite.init("beta-nat-extract",
    nApp(nLam(?(0)), `?n`(0)).compile()
      -->
    BetaNatExtractApplier(?(0), `?n`(0))
  )

  import rise.core.types.{Nat, DataType, Type}
  import NamedRewriteDSL._

  val etaAbstraction = NamedRewrite.init("eta-abstraction",
    ("f" :: (t("a") ->: t("b"))) --> lam("x", app("f", "x"))
  )
  val mapEtaAbstraction = NamedRewrite.init("map-eta-abstraction",
    app(map, "f") --> lam("x", app(app(map, "f"), "x"))
  )

  val eta = NamedRewrite.init("eta",
    lam("x", app("f", "x")) --> "f",
    Seq("f" notFree "x"))

  val removeTransposePair = NamedRewrite.init("remove-transpose-pair",
    app(transpose, app(transpose, "x")) --> "x"
  )

  val fstReduction = NamedRewrite.init("fst-reduction",
    app(fst, app(app(makePair, "a"), "b")) --> "a"
  )

  val sndReduction = NamedRewrite.init("snd-reduction",
    app(snd, app(app(makePair, "a"), "b")) --> "b"
  )

  // TODO: generalize?
  val idxReduction_0_1 = NamedRewrite.init("idx-reduction-0-1",
    app(app(rise.core.primitives.idx.primitive, lidx(0, 1)),
      app(rise.core.primitives.makeArray(1).primitive, "e0"))
      -->
    "e0"
  )
  val idxReduction_0_2 = NamedRewrite.init("idx-reduction-0-2",
    app(app(rise.core.primitives.idx.primitive, lidx(0, 2)),
      app(app(rise.core.primitives.makeArray(2).primitive, "e0"), "e1"))
      -->
    "e0"
  )
  val idxReduction_1_2 = NamedRewrite.init("idx-reduction-1-2",
    app(app(rise.core.primitives.idx.primitive, lidx(1, 2)),
      app(app(rise.core.primitives.makeArray(2).primitive, "e0"), "e1"))
      -->
    "e1"
  )
  val idxReduction_0_3 = NamedRewrite.init("idx-reduction-0-3",
    app(app(rise.core.primitives.idx.primitive, lidx(1, 3)),
      app(app(app(rise.core.primitives.makeArray(3).primitive, "e0"), "e1"), "e2"))
      -->
    "e0"
  )
  val idxReduction_1_3 = NamedRewrite.init("idx-reduction-1-3",
    app(app(rise.core.primitives.idx.primitive, lidx(1, 3)),
      app(app(app(rise.core.primitives.makeArray(3).primitive, "e0"), "e1"), "e2"))
      -->
    "e1"
  )
  val idxReduction_2_3 = NamedRewrite.init("idx-reduction-2-3",
    app(app(rise.core.primitives.idx.primitive, lidx(1, 3)),
      app(app(app(rise.core.primitives.makeArray(3).primitive, "e0"), "e1"), "e2"))
      -->
    "e2"
  )

  // -- algorithmic --

  val mapFusion = NamedRewrite.init("map-fusion",
    app(app(map, "f"), app(app(map, "g"), "in"))
      -->
    app(app(map, lam("x", app("f", app("g", "x")))), "in")
  )
  val mapFission = NamedRewrite.init("map-fission",
    app(map, lam("x", app("f", "gx" :: ("dt": DataType))))
      -->
    lam("in", app(app(map, "f"), app(app(map, lam("x", "gx")), "in"))),
    Seq("f" notFree "x")
  )

  // TODO: other means of picking n, such as tuning parameters
  def splitJoin(n: Int) = NamedRewrite.init(s"split-join-$n",
    /* app(map, "f")
      -->
    lam("x", app(join, app(app(map, app(map, "f")), app(nApp(split, n), "x"))))
     */
    app(app(map, "f"), "in")
      -->
    app(join, app(app(map, app(map, "f")), app(nApp(split, n), "in")))
  )
  def splitJoin1M(n: Int) = NamedRewrite.init(s"split-join-1m-$n",
    app(app(map, app(map, "f")), "in")
      -->
    app(app(map, join), app(app(map, app(map, app(map, "f"))), app(app(map, nApp(split, n)), "in")))
  )
  def splitJoin2M(n: Int) = NamedRewrite.init(s"split-join-2m-$n",
    app(app(map, app(map, app(map, "f"))), "in")
      -->
    app(app(map, app(map, join)), app(app(map, app(map, app(map, app(map, "f")))), app(app(map, app(map, nApp(split, n))), "in")))
  )
  def splitJoin3M(n: Int) = NamedRewrite.init(s"split-join-3m-$n",
    app(app(map, app(map, app(map, app(map, "f")))), "in")
      -->
    app(app(map, app(map, app(map, join))), app(app(map, app(map, app(map, app(map, app(map, "f"))))), app(app(map, app(map, app(map, nApp(split, n)))), "in")))
  )
  def splitJoin4M(n: Int) = NamedRewrite.init(s"split-join-4m-$n",
    app(app(map, app(map, app(map, app(map, app(map, "f"))))), "in")
      -->
    app(app(map, app(map, app(map, app(map, join)))), app(app(map, app(map, app(map, app(map, app(map, app(map, "f")))))), app(app(map, app(map, app(map, app(map, nApp(split, n))))), "in")))
  )
  def splitJoin5M(n: Int) = NamedRewrite.init(s"split-join-5m-$n",
    app(app(map, app(map, app(map, app(map, app(map, app(map, "f")))))), "in")
      -->
    app(app(map, app(map, app(map, app(map, app(map, join))))), app(app(map, app(map, app(map, app(map, app(map, app(map, app(map, "f"))))))), app(app(map, app(map, app(map, app(map, app(map, nApp(split, n)))))), "in")))
  )
  def splitJoin6M(n: Int) = NamedRewrite.init(s"split-join-6m-$n",
    app(app(map, app(map, app(map, app(map, app(map, app(map, app(map, "f"))))))), "in")
      -->
    app(app(map, app(map, app(map, app(map, app(map, app(map, join)))))), app(app(map, app(map, app(map, app(map, app(map, app(map, app(map, app(map, "f")))))))), app(app(map, app(map, app(map, app(map, app(map, app(map, nApp(split, n))))))), "in")))
  )

  def blockedReduce(n: Int) = NamedRewrite.init(s"blocked-reduce-$n",
    app(app(app(reduce, "op" :: ("a" ->: "a" ->: ("a": Type))), "init"), "arg")
      -->
    app(app(app(rcp.reduceSeq.primitive,
      lam("acc", lam("y", app(app("op", "acc"),
        app(app(app(reduce, "op"), "init"), "y"))))),
      "init"), app(nApp(split, n), "arg"))
  )

  val liftReduceSeq = NamedRewrite.init("lift-reduce-seq",
    app(map, app(app(rcp.reduceSeq.primitive, "op"), "init"))
      -->
    lam("in",
      app(app(app(rcp.reduceSeq.primitive, lam("acc", lam("y",
        app(app(map, lam("z", app(app("op", app(fst, "z")), app(snd, "z")))),
          app(app(zip, "acc"), "y"))
      ))), app(rcp.generate.primitive, lam("i", "init"))),
      app(transpose, "in")))
  )

  // FIXME: very specific ..
  val liftReduceSeq2 = NamedRewrite.init("lift-reduce-seq-2",
    app(map, lam("x", app(app(add, app(fst, "x")),
      app(app(app(rcp.reduceSeq.primitive, "op"), lf32(0)), app(snd, "x")))))
      -->
    lam("in", app(lam("uz",
      app(app(app(rcp.reduceSeq.primitive, lam("acc", lam("y",
        app(app(map, lam("z", app(app("op", app(fst, "z")), app(snd, "z")))),
          app(app(zip, "acc"), "y"))
        // generalized init: app(transpose, app(snd, "uz")) + app(rcp.generate.primitive, lam("i", "init"))
      ))), app(fst, "uz")), app(transpose, app(snd, "uz")))),
      app(unzip, "in")))
  )

  // FIXME: very specific ..
  val liftReduceSeq3 = NamedRewrite.init("lift-reduce-seq-3",
    (app(map, lam("x",
      app(app(app(rcp.reduceSeq.primitive, "op"), "i" :: (("m": Nat)`.`("dt1": DataType))),
        "e" :: (("o": Nat)`.`(("m": Nat)`.`("dt2": DataType))))))
      :: ((("n": Nat)`.`(("m": Nat)`.`(("dt1": DataType) x (("o": Nat)`.`("dt2": DataType))))) ->: ("o": Type)))
      -->
    lam("in", app(lam("uz",
      app(app(app(rcp.reduceSeq.primitive, lam("acc", lam("y",
        app(app(map, lam("z", app(app("op", app(fst, "z")), app(snd, "z")))),
          app(app(zip, "acc"), "y"))
      ))), app(fst, "uz")), app(transpose, app(app(map, transpose), app(snd, "uz"))))),
      app(unzip, app(app(map, unzip), "in"))))
  )

  val reduceSeqMapFusion = NamedRewrite.init("reduce-seq-map-fusion",
    app(app(app(rcp.reduceSeq.primitive, "f"), "init"), app(app(map, "g"), "in"))
      -->
    app(app(app(rcp.reduceSeq.primitive, lam("acc", lam("x",
      app(app("f", "acc"), app("g", "x"))))), "init"), "in")
  )

  val reduceSeqMapFission = NamedRewrite.init("reduce-seq-map-fission",
    app(app(rcp.reduceSeq.primitive, lam("acc", lam("y",
      app(app("op", "acc"), "gy" :: ("dt": DataType))))), "init")
      -->
      lam("in", app(app(app(rcp.reduceSeq.primitive, "op"), "init"),
        app(app(map, lam("y", "gy")), "in"))),
    Seq("op" notFree "y")
  )

  val undoReduceSeqForAdd = NamedRewrite.init("undo-reduce-seq-for-add",
    app(rcp.reduceSeq.primitive, add) --> app(reduce, add)
  )

  // FIXME: how can we generalize to N?
  val mapOutsideMakeArray2 = NamedRewrite.init("map-outside-make-array-2",
    app(app(rcp.makeArray(2).primitive, app(app(map, "f1"), "e")), app(app(map, "f2"), "e"))
      -->
    app(transpose, app(app(map, lam("x", app(app(rcp.makeArray(2).primitive, app("f1", "x")), app("f2", "x")))), "e"))
  )

  val mapOutsideZip = NamedRewrite.init("map-outside-zip",
    app(app(zip, app(app(map, "fa"), "a")), app(app(map, "fb"), "b"))
      -->
    app(app(map, lam("p", app(app(makePair,
        app("fa", app(fst, "p"))),
        app("fb", app(snd, "p"))))),
      app(app(zip, "a"), "b"))
  )

  val mapOutsidePair = NamedRewrite.init("map-outside-pair",
    app(app(makePair,
      app(app(map, "fa"), "a" :: (("n": Nat)`.``_`))),
      app(app(map, "fb"), "b" :: (("n": Nat)`.``_`)))
      -->
    app(unzip,
      app(app(map,
        lam("p", app(app(makePair,
          app("fa", app(fst, "p"))),
          app("fb", app(snd, "p"))))),
      app(app(zip, "a"), "b")))
  )

  val zipSame = NamedRewrite.init("zip-same",
    app(app(zip, "a"), "a")
      -->
    app(app(map, lam("x", app(app(makePair, "x"), "x"))), "a")
  )

  val zipSwap = NamedRewrite.init("zip-swap",
    app(app(zip, "a"), "b")
      -->
    app(app(map,
      lam("x", app(app(makePair, app(snd, "x")), app(fst, "x")))),
      app(app(zip, "b"), "a"))
  )

  val zipRotateLeft = NamedRewrite.init("zip-rotate-left",
    app(app(zip, "a"), app(app(zip, "b"), "c"))
      -->
    app(app(map,
      lam("x", app(app(makePair, app(fst, app(fst, "x"))),
        app(app(makePair, app(snd, app(fst, "x"))), app(snd, "x"))))),
      app(app(zip, app(app(zip, "a"), "b")), "c"))
  )

  val zipRotateRight = NamedRewrite.init("zip-rotate-right",
    app(app(zip, app(app(zip, "a"), "b")), "c")
      -->
    app(app(map,
      lam("x", app(app(makePair, app(app(makePair,
        app(fst, "x")), app(fst, app(snd, "x")))), app(snd, app(snd, "x"))))),
      app(app(zip, "a"), app(app(zip, "b"), "c")))
  )

  val idAfter = NamedRewrite.init("id-after",
    ("x": Pattern) --> app(lam("y", "y"), "x")
  )
  val transposePairAfter = NamedRewrite.init("transpose-pair-after",
    ("x" :: ((`_`: Nat)`.`((`_`: Nat)`.``_`))) --> app(transpose, app(transpose, "x"))
  )
  val transposePairAfter2 = NamedRewrite.init("transpose-pair-after-2",
    ("x" :: ((`_`: Nat)`.`((`_`: Nat)`.``_`))) -->
      app(lam("y", app(transpose, app(transpose, "y"))), "x")
  )
  val transposePairAfter3 = NamedRewrite.init("transpose-pair-after-3",
    ("f" :: ("in" ->: ((`_`: Nat)`.`((`_`: Nat)`.``_`)))) -->
      lam("x", app(lam("y", app(transpose, app(transpose, "y"))), app("f", "x")))
  )
  val transposePairAfter4 = NamedRewrite.init("transpose-pair-after-4",
    ("f" :: ("in" ->: ((`_`: Nat)`.`((`_`: Nat)`.``_`)))) -->
    lam("x", app(transpose, app(lam("y", app(transpose, app("f", "y"))), "x")))
  )
  val createTransposePair = NamedRewrite.init("create-transpose-pair",
    app(lam("y", "y"), "x" :: ((`_`: Nat)`.`((`_`: Nat)`.``_`)))
      -->
    app(lam("y", app(transpose, app(transpose, "y"))), "x")
  )

  val eliminateMapIdentity = NamedRewrite.init("eliminate-map-identity",
    app(map, lam("x", "x"))
      -->
    lam("y", "y")
  )
  val mapIdentityAfter = NamedRewrite.init("map-identity-after",
    ("e" :: ((`_`: Nat)`.``_`))
      -->
    app(app(map, lam("x", "x")), "e")
  )

  val fstUnzipAsMapFst = NamedRewrite.init("fst-unzip-as-map-fst",
    app(fst, app(unzip, "e")) --> app(app(map, fst), "e")
  )
  val sndUnzipAsMapSnd = NamedRewrite.init("snd-unzip-as-map-snd",
    app(snd, app(unzip, "e")) --> app(app(map, snd), "e")
  )

  // - slide widening -

  val dropInSlide = NamedRewrite.init("drop-in-slide",
    app(nApp(drop, "l"), app(nApp(nApp(slide, "n"), 1), "in"))
      -->
    app(app(map, nApp(drop, "l")), app(nApp(nApp(slide, ("n": Nat) + ("l": Nat)), 1), "in"))
  )
  val takeInSlide = NamedRewrite.init("take-in-slide",
    app(nApp(take, "r") :: ((("s": Nat)`.``_`) ->: `_`), app(nApp(nApp(slide, "n"), 1), "in"))
      -->
    app(app(map, nApp(take, "n")), app(nApp(nApp(slide, ("n": Nat) + ("s": Nat) - ("r": Nat)), 1), "in"))
  )

  // -- movement --

  val mapSlideBeforeTranspose = NamedRewrite.init("map-slide-before-transpose",
    app(transpose, app(app(map, nApp(nApp(slide, "sz"), "sp")), "in"))
      -->
    app(app(map, transpose), app(nApp(nApp(slide, "sz"), "sp"), app(transpose, "in")))
  )
  val slideBeforeMapMapF = NamedRewrite.init("slide-before-map-map-f",
    app(app(map, app(map, "f")), app(nApp(nApp(slide, "sz"), "sp"), "in"))
      -->
    app(nApp(nApp(slide, "sz"), "sp"), app(app(map, "f"), "in"))
  )
  val slideBeforeMap = NamedRewrite.init("slide-before-map",
    app(nApp(nApp(slide, "sz"), "sp"), app(app(map, "f"), "in"))
      -->
    app(app(map, app(map, "f")), app(nApp(nApp(slide, "sz"), "sp"), "in"))
  )

  val splitBeforeMap = NamedRewrite.init("split-before-map",
    app(nApp(split, "n"), app(app(map, "f"), "in"))
      -->
    app(app(map, app(map, "f")), app(nApp(split, "n"), "in"))
  )

  val mapMapFBeforeTranspose = NamedRewrite.init("map-map-f-before-transpose",
    app(transpose, app(app(map, app(map, "f")), "y"))
      -->
    app(app(map, app(map, "f")), app(transpose, "y"))
  )
  val transposeBeforeMapMapF = NamedRewrite.init("transpose-before-map-map-f",
    app(app(map, app(map, "f")), app(transpose, "y"))
      -->
    app(transpose, app(app(map, app(map, "f")), "y"))
  )

  val transposeAroundMapMapF = NamedRewrite.init("transpose-around-map-map-f",
    app(app(map, app(map, "f")), "in")
      -->
    app(transpose, app(app(map, app(map, "f")), app(transpose, "in")))
  )
  val transposeAroundMapMapF1M = NamedRewrite.init("transpose-around-map-map-f-1m",
    app(app(map, app(map, app(map, "f"))), "in")
      -->
    app(app(map, transpose), app(app(map, app(map, app(map, "f"))), app(app(map, transpose), "in")))
  )
  val transposeAroundMapMapF2M = NamedRewrite.init("transpose-around-map-map-f-2m",
    app(app(map, app(map, app(map, app(map, "f")))), "in")
      -->
    app(app(map, app(map, transpose)), app(app(map, app(map, app(map, app(map, "f")))), app(app(map, app(map, transpose)), "in")))
  )
  val transposeAroundMapMapF3M = NamedRewrite.init("transpose-around-map-map-f-3m",
    app(app(map, app(map, app(map, app(map, app(map, "f"))))), "in")
      -->
    app(app(map, app(map, app(map, transpose))), app(app(map, app(map, app(map, app(map, app(map, "f"))))), app(app(map, app(map, app(map, transpose))), "in")))
  )
  val transposeAroundMapMapF4M = NamedRewrite.init("transpose-around-map-map-f-4m",
    app(app(map, app(map, app(map, app(map, app(map, app(map, "f")))))), "in")
      -->
    app(app(map, app(map, app(map, app(map, transpose)))), app(app(map, app(map, app(map, app(map, app(map, app(map, "f")))))), app(app(map, app(map, app(map, app(map, transpose)))), "in")))
  )
  val transposeAroundMapMapF5M = NamedRewrite.init("transpose-around-map-map-f-5m",
    app(app(map, app(map, app(map, app(map, app(map, app(map, app(map, "f"))))))), "in")
      -->
    app(app(map, app(map, app(map, app(map, app(map, transpose))))), app(app(map, app(map, app(map, app(map, app(map, app(map, app(map, "f"))))))), app(app(map, app(map, app(map, app(map, app(map, transpose))))), "in")))
  )

  val dropBeforeMap = NamedRewrite.init("drop-before-map",
    app(nApp(drop, "n"), app(app(map, "f"), "in"))
      -->
    app(app(map, "f"), app(nApp(drop, "n"), "in"))
  )
  val takeBeforeMap = NamedRewrite.init("take-before-map",
    app(nApp(take, "n"), app(app(map, "f"), "in"))
      -->
    app(app(map, "f"), app(nApp(take, "n"), "in"))
  )

  val dropBeforeTake = NamedRewrite.init("drop-before-take",
    app(nApp(drop, "m"), app(nApp(take, "n+m"), "in"))
      -->
    app(nApp(take, ("n+m": Nat) - ("m": Nat)), app(nApp(drop, "m"), "in"))
  )
  val takeBeforeDrop = NamedRewrite.init("take-before-drop",
    app(nApp(take, "n"), app(nApp(drop, "m"), "in"))
      -->
    app(nApp(drop, "m"), app(nApp(take, "n" + "m"), "in"))
  )

  val slideInsideZip = NamedRewrite.init("slide-inside-zip",
    app(nApp(nApp(slide, "n"), "m"), app(app(zip, "a"), "b"))
      -->
    app(app(map, lam("p", app(app(zip, app(fst, "p")), app(snd, "p")))),
      app(app(zip,
        app(nApp(nApp(slide, "n"), "m"), "a")),
        app(nApp(nApp(slide, "n"), "m"), "b")))
  )

  val slideOutsideZip = NamedRewrite.init("slide-outside-zip",
    app(app(zip,
      app(nApp(nApp(slide, "n"), "m"), "a")),
      app(nApp(nApp(slide, "n"), "m"), "b"))
      -->
    app(app(map, unzip),
      app(nApp(nApp(slide, "n"), "m"), app(app(zip, "a"), "b")))
  )

  val takeOutsidePair = NamedRewrite.init("take-outside-pair",
    app(app(makePair, app(nApp(take, "n"), "a")), app(nApp(take, "m"), "b"))
      -->
    app(app(mapSnd, nApp(take, "m")),
      app(app(mapFst, nApp(take, "n")),
        app(app(makePair, "a"), "b")))
  )

  // -- lowering --

  val reduceSeq = NamedRewrite.init("reduce-seq",
    reduce --> rcp.reduceSeq.primitive
  )
  val reduceSeqUnroll = NamedRewrite.init("reduce-seq-unroll",
    reduce --> rcp.reduceSeqUnroll.primitive
  )
  val mapSeq = NamedRewrite.init("map-seq",
    map --> rcp.mapSeq.primitive
  )
  val iterateStream = NamedRewrite.init("iterate-stream",
    map --> rcp.iterateStream.primitive
  )
  val toMemAfterMapSeq = NamedRewrite.init("to-mem-after-map-seq",
    app(app(rcp.mapSeq.primitive, "f"), "in")
      -->
    app(rcp.toMem.primitive, app(app(rcp.mapSeq.primitive, "f"), "in"))
  )

  val mapSeqArray = NamedRewrite.init("map-seq-array",
    ("x" :: ((`_`: Nat)`.`(`_`: DataType)))
      -->
    app(app(rcp.mapSeq.primitive, lam("y", "y")), "x")
  )

  // TODO: condition typeHasTrivialCopy(t) / or synthesise trivial write function
  val mapSeqUnrollWrite = NamedRewrite.init("map-seq-unroll-write",
    ("x" :: ((`_`: Nat)`.`"dt"))
      -->
    app(app(rcp.mapSeqUnroll.primitive, lam("y", "y")), "x")
  )
  val mapSeqUnrollMapSeqWrite = NamedRewrite.init("map-seq-unroll-map-seq-write",
    ("x" :: ((`_`: Nat)`.`((`_`: Nat)`.`"dt")))
      -->
      app(app(rcp.mapSeqUnroll.primitive, app(rcp.mapSeq.primitive, lam("y", "y"))), "x")
  )

  val circularBuffer = NamedRewrite.init("circular-buffer",
    nApp(nApp(slide, "sz"), 1) --> app(nApp(nApp(rcp.circularBuffer.primitive, "sz"), "sz"), lam("x", "x"))
  )
  val circularBufferLoadFusion = NamedRewrite.init("circular-buffer-load-fusion",
    app(app(nApp(nApp(rcp.circularBuffer.primitive, "n1"), "n2"), "load"), app(app(map, "f"), "in"))
      -->
      app(app(nApp(nApp(rcp.circularBuffer.primitive, "n1"), "n2"), lam("x", app("load", app("f", "x")))), "in")
  )

  // TODO: generalize to rotateValues(write: Expr)
  val rotateValuesScalar = NamedRewrite.init("rotate-values-scalar",
    nApp(nApp(slide, "sz"), 1) --> app(nApp(rcp.rotateValues.primitive, "sz"), lam("x", "x"))
  )

  object vectorize {
    val asScalarOutsidePair = NamedRewrite.init("as-scalar-outside-pair",
      app(app(makePair, app(asScalar, "a")), app(asScalar, "b"))
        -->
      app(app(mapSnd, asScalar), app(app(mapFst, asScalar),
        app(app(makePair, "a"), "b")))
    )
  }

  object ocl {
    import rise.openCL.{primitives => p}

    /* TODO: aApp
    val circularBuffer = NamedRewrite.init("ocl-circular-buffer",
      nApp(nApp(slide, "sz"), 1) --> app(nApp(nApp(p.oclCircularBuffer.primitive, "sz"), "sz"), lam("x", "x"))
    )
    val circularBufferLoadFusion = NamedRewrite.init("ocl-circular-buffer-load-fusion",
      app(app(nApp(nApp(p.oclCircularBuffer.primitive, "n1"), "n2"), "load"), app(app(map, "f"), "in"))
        -->
      app(app(nApp(nApp(p.oclCircularBuffer.primitive, "n1"), "n2"), lam("x", app("load", app("f", "x")))), "in")
    )
    val reduceSeq = NamedRewrite.init("ocl-reduce-seq",
      reduce --> p.oclReduceSeq.primitive
    )
    val reduceSeqUnroll = NamedRewrite.init("ocl-reduce-seq-unroll",
      rcp.reduceSeq.primitive --> p.oclReduceSeqUnroll.primitive
    )

     */
  }

  object combinatory {
    val compositionIntro = NamedRewrite.init("composition-intro",
      // only do this for functions over datatypes
      app("f" :: (("dt2": DataType) ->: ("dt3" : DataType)),
        app("g" :: (("dt1": DataType) ->: ("dt2" : DataType)), "x")) --> app("g" >> "f", "x")
    )
    val compositionElim = NamedRewrite.init("composition-elim",
      app("g" >> "f", "x") --> app("f", app("g", "x"))
    )
    val compositionAssoc1 = NamedRewrite.init("composition-assoc-1",
      (("f" >> "g") >> "h") --> ("f" >> ("g" >> "h"))
    )
    val compositionAssoc2 = NamedRewrite.init("composition-assoc-2",
      ("f" >> ("g" >> "h")) --> (("f" >> "g") >> "h")
    )

    val mapFusion = NamedRewrite.init("map-fusion-cnf",
      (app(map, "f") >> app(map, "g"))
        -->
      app(map, "f" >> "g")
    )
    val mapFusion2 = NamedRewrite.init("map-fusion-cnf-2",
      (("h" >> app(map, "f")) >> app(map, "g"))
        -->
      ("h" >> app(map, "f" >> "g"))
    )
    val mapFission = NamedRewrite.init("map-fission-cnf",
      app(map, ("f" :: (("a": DataType) ->: ("b": DataType))) >> "g")
        -->
      (app(map, "f") >> app(map, "g"))
    )

    val reduceSeqMapFusion = NamedRewrite.init("reduce-seq-map-fusion-cnf",
      (app(map, "g") >> app(app(rcp.reduceSeq.primitive, "f"), "init"))
        -->
      app(app(rcp.reduceSeq.primitive,
        lam("acc", lam("x", app(app("f", "acc"), app("g", "x"))))),
        // lam("acc", "g" >> app("f", "acc"))),
        // lam("acc", lam("x", app("g" >> app("f", "acc"), "x")))),
        "init")
    )

    val reduceSeqMapFusion2 = NamedRewrite.init("reduce-seq-map-fusion-cnf-2",
      (("h" >> app(map, "g")) >> app(app(rcp.reduceSeq.primitive, "f"), "init"))
        -->
        ("h" >> app(app(rcp.reduceSeq.primitive,
          lam("acc", lam("x", app(app("f", "acc"), app("g", "x"))))),
          // lam("acc", "g" >> app("f", "acc"))),
          // lam("acc", lam("x", app("g" >> app("f", "acc"), "x")))),
          "init"))
    )

    val reduceSeqMapFission = NamedRewrite.init("reduce-seq-map-fission-cnf",
      app(app(rcp.reduceSeq.primitive, lam("acc", lam("y",
        app(app("op", "acc"), "gy" :: ("dt": DataType))))), "init")
        -->
        lam("in", app(app(app(rcp.reduceSeq.primitive, "op"), "init"),
          app(app(map, lam("y", "gy")), "in"))),
      Seq("op" notFree "y")
    )

    def splitJoin(n: Int) = NamedRewrite.init(s"split-join-cnf-$n",
      app(map, "f")
        -->
      (nApp(split, n) >> app(map, app(map, "f")) >> join)
    )
    def splitJoin1M(n: Int) = NamedRewrite.init(s"split-join-1m-cnf-$n",
      app(map, app(map, "f"))
        -->
      (app(map, nApp(split, n)) >> app(map, app(map, app(map, "f"))) >> app(map, join))
    )
    def splitJoin2M(n: Int) = NamedRewrite.init(s"split-join-2m-cnf-$n",
      app(map, app(map, app(map, "f")))
        -->
      (app(map, app(map, nApp(split, n))) >> app(map, app(map, app(map, app(map, "f")))) >> app(map, app(map, join)))
    )
    def splitJoin3M(n: Int) = NamedRewrite.init(s"split-join-3m-cnf-$n",
      app(map, app(map, app(map, app(map, "f"))))
        -->
      (app(map, app(map, app(map, nApp(split, n)))) >> app(map, app(map, app(map, app(map, app(map, "f"))))) >> app(map, app(map, app(map, join))))
    )
    def splitJoin4M(n: Int) = NamedRewrite.init(s"split-join-4m-cnf-$n",
      app(map, app(map, app(map, app(map, app(map, "f")))))
        -->
      (app(map, app(map, app(map, app(map, nApp(split, n))))) >> app(map, app(map, app(map, app(map, app(map, app(map, "f")))))) >> app(map, app(map, app(map, app(map, join)))))
    )
    def splitJoin5M(n: Int) = NamedRewrite.init(s"split-join-5m-cnf-$n",
      app(map, app(map, app(map, app(map, app(map, app(map, "f"))))))
        -->
      (app(map, app(map, app(map, app(map, app(map, nApp(split, n)))))) >> app(map, app(map, app(map, app(map, app(map, app(map, app(map, "f"))))))) >> app(map, app(map, app(map, app(map, app(map, join))))))
    )
    def splitJoin6M(n: Int) = NamedRewrite.init(s"split-join-6m-cnf-$n",
      app(map, app(map, app(map, app(map, app(map, app(map, app(map, "f")))))))
        -->
      (app(map, app(map, app(map, app(map, app(map, app(map, nApp(split, n))))))) >> app(map, app(map, app(map, app(map, app(map, app(map, app(map, app(map, "f")))))))) >> app(map, app(map, app(map, app(map, app(map, app(map, join)))))))
    )
    def blockedReduce(n: Int) = NamedRewrite.init(s"blocked-reduce-cnf-$n",
      app(app(reduce, "op" :: ("a" ->: "a" ->: ("a": Type))), "init")
        -->
      (nApp(split, n) >> app(app(rcp.reduceSeq.primitive,
        lam("acc", lam("y", app(app("op", "acc"),
          app(app(app(reduce, "op"), "init"), "y"))))),
        "init"))
    )

    val liftReduceSeq = NamedRewrite.init("lift-reduce-seq-cnf",
      app(map, app(app(rcp.reduceSeq.primitive, "op"), "init"))
        -->
      (transpose >>
      app(app(rcp.reduceSeq.primitive, lam("acc", lam("y",
        app(app(map, lam("z", app(app("op", app(fst, "z")), app(snd, "z")))),
          app(app(zip, "acc"), "y"))
      ))), app(rcp.generate.primitive, lam("i", "init"))))
    )

    // FIXME: very specific ..
    val liftReduceSeq2 = NamedRewrite.init("lift-reduce-seq-cnf-2",
      app(map, lam("x", app(app(add, app(fst, "x")),
        app(app(app(rcp.reduceSeq.primitive, "op"), lf32(0)), app(snd, "x")))))
        -->
      (unzip >> lam("uz",
        app(app(app(rcp.reduceSeq.primitive, lam("acc", lam("y",
          app(app(map, lam("z", app(app("op", app(fst, "z")), app(snd, "z")))),
            app(app(zip, "acc"), "y"))
          // generalized init: app(transpose, app(snd, "uz")) + app(rcp.generate.primitive, lam("i", "init"))
        ))), app(fst, "uz")), app(transpose, app(snd, "uz")))))
    )

    val removeTransposePair = NamedRewrite.init("remove-transpose-pair-cnf",
      (transpose >> transpose) --> lam("x", "x")
    )
    val removeTransposePair2 = NamedRewrite.init("remove-transpose-pair-cnf-2",
      (("g" >> transpose) >> transpose) --> "g"
    )
    val removeTransposePair1M = NamedRewrite.init("remove-transpose-pair-cnf-1m",
      (app(map, transpose) >> app(map, transpose)) --> lam("x", "x")
    )
    val removeTransposePair1M2 = NamedRewrite.init("remove-transpose-pair-cnf-1m-2",
      (("g" >> app(map, transpose)) >> app(map, transpose)) --> "g"
    )
    val removeTransposePair2M = NamedRewrite.init("remove-transpose-pair-cnf-2m",
      (app(map, app(map, transpose)) >> app(map, app(map, transpose))) --> lam("x", "x")
    )
    val removeTransposePair2M2 = NamedRewrite.init("remove-transpose-pair-cnf-2m-2",
      (("g" >> app(map, app(map, transpose))) >> app(map, app(map, transpose))) --> "g"
    )
    val compositionRightId = NamedRewrite.init("composition-right-id",
      ("f" >> lam("x", "x")) --> "f"
    )
    val compositionLeftId = NamedRewrite.init("composition-left-id",
      (lam("x", "x") >> "f") --> "f"
    )

    val mapSlideBeforeTranspose = NamedRewrite.init("map-slide-before-transpose-cnf",
      (app(map, nApp(nApp(slide, "sz"), "sp")) >> transpose)
        -->
      (transpose >> nApp(nApp(slide, "sz"), "sp") >> app(map, transpose))
    )
    val slideBeforeMapMapF = NamedRewrite.init("slide-before-map-map-f-cnf",
      (nApp(nApp(slide, "sz"), "sp") >> app(map, app(map, "f")))
        -->
      (app(map, "f") >> nApp(nApp(slide, "sz"), "sp"))
    )
    val slideBeforeMap = NamedRewrite.init("slide-before-map-cnf",
      (app(map, "f") >> nApp(nApp(slide, "sz"), "sp"))
        -->
      (nApp(nApp(slide, "sz"), "sp") >> app(map, app(map, "f")))
    )

    val splitBeforeMap = NamedRewrite.init("split-before-map-cnf",
      (app(map, "f") >> nApp(split, "n"))
        -->
      (nApp(split, "n") >> app(map, app(map, "f")))
    )
    val splitBeforeMap2 = NamedRewrite.init("split-before-map-cnf-2",
      (("g" >> app(map, "f")) >> nApp(split, "n"))
        -->
      ("g" >> nApp(split, "n") >> app(map, app(map, "f")))
    )

    val transposePairAfter = NamedRewrite.init("transpose-pair-after-cnf",
      ("f" :: (t("in") ->: ((`_`: Nat)`.`((`_`: Nat)`.``_`)))) -->
      ("f" >> transpose >> transpose)
    )

    val mapMapFBeforeTranspose = NamedRewrite.init("map-map-f-before-transpose-cnf",
      (app(map, app(map, "f")) >> transpose)
        -->
      (transpose >> app(map, app(map, "f")))
    )
    val mapMapFBeforeTranspose1 = NamedRewrite.init("map-map-f-before-transpose-cnf-1",
      ("g" >> app(map, app(map, "f")) >> transpose)
        -->
      ("g" >> transpose >> app(map, app(map, "f")))
    )
    val mapMapFBeforeTranspose2 = NamedRewrite.init("map-map-f-before-transpose-cnf-2",
      (app(map, app(map, "f")) >> (transpose >> "g"))
        -->
      (transpose >> (app(map, app(map, "f")) >> "g"))
    )

    val transposeAroundMapMapF = NamedRewrite.init("transpose-around-map-map-f-cnf",
      app(map, app(map, "f"))
        -->
      (transpose >> app(map, app(map, "f")) >> transpose)
    )

    val transposeAroundMapMapF1M = NamedRewrite.init("transpose-around-map-map-f-1m-cnf",
      app(map, app(map, app(map, "f")))
        -->
      (app(map, transpose) >> app(map, app(map, app(map, "f"))) >> app(map, transpose))
    )

    val transposeAroundMapMapF2M = NamedRewrite.init("transpose-around-map-map-f-2m-cnf",
      app(map, app(map, app(map, app(map, "f"))))
        -->
      (app(map, app(map, transpose)) >> app(map, app(map, app(map, app(map, "f")))) >> app(map, app(map, transpose)))
    )

    val transposeAroundMapMapF3M = NamedRewrite.init("transpose-around-map-map-f-3m-cnf",
      app(map, app(map, app(map, app(map, app(map, "f")))))
        -->
      (app(map, app(map, app(map, transpose))) >> app(map, app(map, app(map, app(map, app(map, "f"))))) >> app(map, app(map, app(map, transpose))))
    )

    val transposeAroundMapMapF4M = NamedRewrite.init("transpose-around-map-map-f-4m-cnf",
      app(map, app(map, app(map, app(map, app(map, app(map, "f"))))))
        -->
      (app(map, app(map, app(map, app(map, transpose)))) >> app(map, app(map, app(map, app(map, app(map, app(map, "f")))))) >> app(map, app(map, app(map, app(map, transpose)))))
    )

    val transposeAroundMapMapF5M = NamedRewrite.init("transpose-around-map-map-f-5m-cnf",
      app(map, app(map, app(map, app(map, app(map, app(map, app(map, "f")))))))
        -->
      (app(map, app(map, app(map, app(map, app(map, transpose))))) >> app(map, app(map, app(map, app(map, app(map, app(map, app(map, "f"))))))) >> app(map, app(map, app(map, app(map, app(map, transpose))))))
    )

    object vectorize {
      // TODO: `s` should be a scalar type and `m % n == 0`
      def after(n: Nat) = NamedRewrite.init(s"vec-after-$n-cnf",
        ("e" :: (("m": Nat)`.`("s": DataType)))
          -->
        ("e" >> nApp(asVector, n) >> asScalar)
      )

      val asScalarAsVectorId = NamedRewrite.init("vec-as-scalar-as-vector-id-cnf",
        ((asScalar >> nApp(asVector, "n")) :: (("t": Type) ->: ("t": Type)))
          -->
        lam("in", "in")
      )
      val asScalarAsVectorId2 = NamedRewrite.init("vec-as-scalar-as-vector-id-cnf-2",
        ("h" >> (asScalar >> nApp(asVector, "n")) :: (("t": Type) ->: ("t": Type)))
          -->
        "h"
      )

      // TODO: check that "f" is vectorizable / synthesize vectorized "f" ?
      /*
      val beforeMap = NamedRewrite.init("vec-before-map",
        (app(map, "f") >> nApp(asVector, "n"))
          -->
        (nApp(asVector, "n") >> app(map, "f"))
      )

       */
    }
  }
}
