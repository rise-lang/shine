package rise.eqsat

import PatternDSL._
import rise.core.{primitives => rcp}
import rise.eqsat.NamedRewriteDSL.`_`

object rules {
  type Rule = DefaultAnalysis.Rewrite

  def containsIdent(v: PatternVar, ident: Var)
                   (egraph: DefaultAnalysis.EGraph,
                    eclass: EClassId,
                    shc: SubstHashCons,
                    subst: Subst): Boolean =
    egraph.getMut(subst(v, shc)).data.free.contains(ident.index)

  def neg[ED, ND, TD](cond: (EGraph[ED, ND, TD], EClassId, Subst) => Boolean)
                     (egraph: EGraph[ED, ND, TD], eclass: EClassId, subst: Subst): Boolean =
    !cond(egraph, eclass, subst)

  // TODO: find a way to combine different analysis requirements?

  // -- reduction --

  val beta: Rule = Rewrite.init("beta",
    app(lam(?(0)), ?(1)).compile()
      -->
    BetaApplier(?(0), ?(1))
  )
  val betaNat: Rule = Rewrite.init("beta-nat",
    nApp(nLam(?(0)), `?n`(0)).compile()
      -->
    BetaNatApplier(?(0), `?n`(0))
  )
  val betaExtract: Rule = Rewrite.init("beta-extract",
    app(lam(?(0)), ?(1)).compile()
      -->
    BetaExtractApplier(?(0), ?(1))
  )
  val betaNatExtract: Rule = Rewrite.init("beta-nat-extract",
    nApp(nLam(?(0)), `?n`(0)).compile()
      -->
    BetaNatExtractApplier(?(0), `?n`(0))
  )

  import rise.core.types.{Nat, DataType, Type}
  import NamedRewriteDSL._

  val etaAbstraction: Rule = NamedRewrite.init("eta-abstraction",
    ("f" :: (t("a") ->: t("b"))) --> lam("x", app("f", "x"))
  )

  val eta: Rule = NamedRewrite.init("eta",
    lam("x", app("f", "x")) --> "f",
    Seq("f" notFree "x"))
  val removeTransposePair: Rule = NamedRewrite.init("remove-transpose-pair",
    app(transpose, app(transpose, "x")) --> "x"
  )

  val fstReduction: Rule = NamedRewrite.init("fst-reduction",
    app(fst, app(app(makePair, "a"), "b")) --> "a"
  )

  val sndReduction: Rule = NamedRewrite.init("snd-reduction",
    app(snd, app(app(makePair, "a"), "b")) --> "b"
  )

  /* TODO
  val idxReduction: Rule = NamedRewrite.init("idx-reduction",
    app(app(rise.core.primitives.idx.primitive, "i"), "mka")
      -->
    ???
  )
  */

  // -- algorithmic --

  val mapFusion: Rule = NamedRewrite.init("map-fusion",
    app(app(map, "f"), app(app(map, "g"), "in"))
      -->
    app(app(map, lam("x", app("f", app("g", "x")))), "in")
  )
  val mapFission: Rule = NamedRewrite.init("map-fission",
    app(map, lam("x", app("f", "gx" :: ("dt": DataType))))
      -->
    lam("in", app(app(map, "f"), app(app(map, lam("x", "gx")), "in"))),
    Seq("f" notFree "x")
  )

  // TODO: other means of picking n, such as tuning parameters
  def splitJoin(n: Int): Rule = NamedRewrite.init(s"split-join-$n",
    app(map, "f")
      -->
    lam("x", app(join, app(app(map, app(map, "f")), app(nApp(split, n), "x"))))
  )
  def blockedReduce(n: Int): Rule = NamedRewrite.init(s"blocked-reduce-$n",
    app(app(app(reduce, "op" :: ("a" ->: "a" ->: ("a": Type))), "init"), "arg")
      -->
    app(app(app(rcp.reduceSeq.primitive,
      lam("acc", lam("y", app(app("op", "acc"),
        app(app(app(reduce, "op"), "init"), "y"))))),
      "init"), app(nApp(split, n), "arg"))
  )

  val liftReduceSeq: Rule = NamedRewrite.init("lift-reduce-seq",
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
  val liftReduceSeq2: Rule = NamedRewrite.init("lift-reduce-seq-2",
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

  val reduceSeqMapFusion: Rule = NamedRewrite.init("reduce-seq-map-fusion",
    app(app(app(rcp.reduceSeq.primitive, "f"), "init"), app(app(map, "g"), "in"))
      -->
    app(app(app(rcp.reduceSeq.primitive, lam("acc", lam("x",
      app(app("f", "acc"), app("g", "x"))))), "init"), "in")
  )

  val reduceSeqMapFission: Rule = NamedRewrite.init("reduce-seq-map-fission",
    app(app(rcp.reduceSeq.primitive, lam("acc", lam("y",
      app(app("op", "acc"), "gy" :: ("dt": DataType))))), "init")
      -->
      lam("in", app(app(app(rcp.reduceSeq.primitive, "op"), "init"),
        app(app(map, lam("y", "gy")), "in"))),
    Seq("op" notFree "y")
  )

  val undoReduceSeqForAdd: Rule = NamedRewrite.init("undo-reduce-seq-for-add",
    app(rcp.reduceSeq.primitive, add) --> app(reduce, add)
  )

  // FIXME: how can we generalize to N?
  val mapOutsideMakeArray2: Rule = NamedRewrite.init("map-outside-make-array-2",
    app(app(rcp.makeArray(2).primitive, app(app(map, "f1"), "e")), app(app(map, "f2"), "e"))
      -->
    app(transpose, app(app(map, lam("x", app(app(rcp.makeArray(2).primitive, app("f1", "x")), app("f2", "x")))), "e"))
  )

  val mapOutsideZip: Rule = NamedRewrite.init("map-outside-zip",
    app(app(zip, app(app(map, "fa"), "a")), app(app(map, "fb"), "b"))
      -->
    app(app(map, lam("p", app(app(makePair,
        app("fa", app(fst, "p"))),
        app("fb", app(snd, "p"))))),
      app(app(zip, "a"), "b"))
  )

  val mapOutsidePair: Rule = NamedRewrite.init("map-outside-pair",
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

  val zipSame: Rule = NamedRewrite.init("zip-same",
    app(app(zip, "a"), "a")
      -->
    app(app(map, lam("x", app(app(makePair, "x"), "x"))), "a")
  )

  val zipSwap: Rule = NamedRewrite.init("zip-swap",
    app(app(zip, "a"), "b")
      -->
    app(app(map,
      lam("x", app(app(makePair, app(snd, "x")), app(fst, "x")))),
      app(app(zip, "b"), "a"))
  )

  val zipRotateLeft: Rule = NamedRewrite.init("zip-rotate-left",
    app(app(zip, "a"), app(app(zip, "b"), "c"))
      -->
    app(app(map,
      lam("x", app(app(makePair, app(fst, app(fst, "x"))),
        app(app(makePair, app(snd, app(fst, "x"))), app(snd, "x"))))),
      app(app(zip, app(app(zip, "a"), "b")), "c"))
  )

  val zipRotateRight: Rule = NamedRewrite.init("zip-rotate-right",
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
  val transposePairAfter: Rule = NamedRewrite.init("transpose-pair-after",
    ("x" :: ((`_`: Nat)`.`((`_`: Nat)`.``_`))) --> app(transpose, app(transpose, "x"))
  )
  val transposePairAfter2: Rule = NamedRewrite.init("transpose-pair-after-2",
    ("x" :: ((`_`: Nat)`.`((`_`: Nat)`.``_`))) -->
      app(lam("y", app(transpose, app(transpose, "y"))), "x")
  )
  val transposePairAfter3: Rule = NamedRewrite.init("transpose-pair-after-3",
    ("f" :: ("in" ->: ((`_`: Nat)`.`((`_`: Nat)`.``_`)))) -->
      lam("x", app(lam("y", app(transpose, app(transpose, "y"))), app("f", "x")))
  )
  val transposePairAfter4: Rule = NamedRewrite.init("transpose-pair-after-4",
    ("f" :: ("in" ->: ((`_`: Nat)`.`((`_`: Nat)`.``_`)))) -->
    lam("x", app(transpose, app(lam("y", app(transpose, app("f", "y"))), "x")))
  )
  val createTransposePair: Rule = NamedRewrite.init("create-transpose-pair",
    app(lam("y", "y"), "x" :: ((`_`: Nat)`.`((`_`: Nat)`.``_`)))
      -->
    app(lam("y", app(transpose, app(transpose, "y"))), "x")
  )

  val mapIdentityAfter: Rule = NamedRewrite.init("map-identity-after",
    ("e" :: ((`_`: Nat)`.``_`))
      -->
    app(app(map, lam("x", "x")), "e")
  )

  // - slide widening -

  val dropInSlide: Rule = NamedRewrite.init("drop-in-slide",
    app(nApp(drop, "l"), app(nApp(nApp(slide, "n"), 1), "in"))
      -->
    app(app(map, nApp(drop, "l")), app(nApp(nApp(slide, ("n": Nat) + ("l": Nat)), 1), "in"))
  )
  val takeInSlide: Rule = NamedRewrite.init("take-in-slide",
    app(nApp(take, "r") :: ((("s": Nat)`.``_`) ->: `_`), app(nApp(nApp(slide, "n"), 1), "in"))
      -->
    app(app(map, nApp(take, "n")), app(nApp(nApp(slide, ("n": Nat) + ("s": Nat) - ("r": Nat)), 1), "in"))
  )

  // -- movement --

  val mapSlideBeforeTranspose: Rule = NamedRewrite.init("map-slide-before-transpose",
    app(transpose, app(app(map, nApp(nApp(slide, "sz"), "sp")), "in"))
      -->
    app(app(map, transpose), app(nApp(nApp(slide, "sz"), "sp"), app(transpose, "in")))
  )
  val slideBeforeMapMapF: Rule = NamedRewrite.init("slide-before-map-map-f",
    app(app(map, app(map, "f")), app(nApp(nApp(slide, "sz"), "sp"), "in"))
      -->
    app(nApp(nApp(slide, "sz"), "sp"), app(app(map, "f"), "in"))
  )
  val slideBeforeMap: Rule = NamedRewrite.init("slide-before-map",
    app(nApp(nApp(slide, "sz"), "sp"), app(app(map, "f"), "in"))
      -->
    app(app(map, app(map, "f")), app(nApp(nApp(slide, "sz"), "sp"), "in"))
  )

  val splitBeforeMap: Rule = NamedRewrite.init("split-before-map",
    app(nApp(split, "n"), app(app(map, "f"), "in"))
      -->
    app(app(map, app(map, "f")), app(nApp(split, "n"), "in"))
  )

  val mapMapFBeforeTranspose: Rule = NamedRewrite.init("map-map-f-before-transpose",
    app(transpose, app(app(map, app(map, "f")), "y"))
      -->
    app(app(map, app(map, "f")), app(transpose, "y"))
  )
  val transposeBeforeMapMapF: Rule = NamedRewrite.init("transpose-before-map-map-f",
    app(app(map, app(map, "f")), app(transpose, "y"))
      -->
    app(transpose, app(app(map, app(map, "f")), "y"))
  )

  val dropBeforeMap: Rule = NamedRewrite.init("drop-before-map",
    app(nApp(drop, "n"), app(app(map, "f"), "in"))
      -->
    app(app(map, "f"), app(nApp(drop, "n"), "in"))
  )
  val takeBeforeMap: Rule = NamedRewrite.init("take-before-map",
    app(nApp(take, "n"), app(app(map, "f"), "in"))
      -->
    app(app(map, "f"), app(nApp(take, "n"), "in"))
  )

  val dropBeforeTake: Rule = NamedRewrite.init("drop-before-take",
    app(nApp(drop, "m"), app(nApp(take, "n+m"), "in"))
      -->
    app(nApp(take, ("n+m": Nat) - ("m": Nat)), app(nApp(drop, "m"), "in"))
  )

  val slideInsideZip: Rule = NamedRewrite.init("slide-inside-zip",
    app(nApp(nApp(slide, "n"), "m"), app(app(zip, "a"), "b"))
      -->
    app(app(map, lam("p", app(app(zip, app(fst, "p")), app(snd, "p")))),
      app(app(zip,
        app(nApp(nApp(slide, "n"), "m"), "a")),
        app(nApp(nApp(slide, "n"), "m"), "b")))
  )

  val slideOutsideZip: Rule = NamedRewrite.init("slide-outside-zip",
    app(app(zip,
      app(nApp(nApp(slide, "n"), "m"), "a")),
      app(nApp(nApp(slide, "n"), "m"), "b"))
      -->
    app(app(map, unzip),
      app(nApp(nApp(slide, "n"), "m"), app(app(zip, "a"), "b")))
  )

  val takeOutsidePair: Rule = NamedRewrite.init("take-outside-pair",
    app(app(makePair, app(nApp(take, "n"), "a")), app(nApp(take, "m"), "b"))
      -->
    app(app(mapSnd, nApp(take, "m")),
      app(app(mapFst, nApp(take, "n")),
        app(app(makePair, "a"), "b")))
  )

  // -- lowering --

  val reduceSeq: Rule = NamedRewrite.init("reduce-seq",
    reduce --> rcp.reduceSeq.primitive
  )
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

  // TODO: condition typeHasTrivialCopy(t) / or synthesise trivial write function
  val mapSeqUnrollWrite: Rule = NamedRewrite.init("map-seq-unroll-write",
    ("x" :: ((`_`: Nat)`.`"dt"))
      -->
    app(app(rcp.mapSeqUnroll.primitive, lam("y", "y")), "x")
  )
  val mapSeqUnrollMapSeqWrite: Rule = NamedRewrite.init("map-seq-unroll-map-seq-write",
    ("x" :: ((`_`: Nat)`.`((`_`: Nat)`.`"dt")))
      -->
      app(app(rcp.mapSeqUnroll.primitive, app(rcp.mapSeq.primitive, lam("y", "y"))), "x")
  )

  val circularBuffer: Rule = NamedRewrite.init("circular-buffer",
    nApp(nApp(slide, "sz"), 1) --> app(nApp(nApp(rcp.circularBuffer.primitive, "sz"), "sz"), lam("x", "x"))
  )
  val circularBufferLoadFusion: Rule = NamedRewrite.init("circular-buffer-load-fusion",
    app(app(nApp(nApp(rcp.circularBuffer.primitive, "n1"), "n2"), "load"), app(app(map, "f"), "in"))
      -->
      app(app(nApp(nApp(rcp.circularBuffer.primitive, "n1"), "n2"), lam("x", app("load", app("f", "x")))), "in")
  )

  // TODO: generalize to rotateValues(write: Expr)
  val rotateValuesScalar: Rule = NamedRewrite.init("rotate-values-scalar",
    nApp(nApp(slide, "sz"), 1) --> app(nApp(rcp.rotateValues.primitive, "sz"), lam("x", "x"))
  )

  object vectorize {
    val asScalarOutsidePair: Rule = NamedRewrite.init("as-scalar-outside-pair",
      app(app(makePair, app(asScalar, "a")), app(asScalar, "b"))
        -->
      app(app(mapSnd, asScalar), app(app(mapFst, asScalar),
        app(app(makePair, "a"), "b")))
    )
  }

  object ocl {
    import rise.openCL.{primitives => p}

    /* TODO: aApp
    val circularBuffer: Rule = NamedRewrite.init("ocl-circular-buffer",
      nApp(nApp(slide, "sz"), 1) --> app(nApp(nApp(p.oclCircularBuffer.primitive, "sz"), "sz"), lam("x", "x"))
    )
    val circularBufferLoadFusion: Rule = NamedRewrite.init("ocl-circular-buffer-load-fusion",
      app(app(nApp(nApp(p.oclCircularBuffer.primitive, "n1"), "n2"), "load"), app(app(map, "f"), "in"))
        -->
      app(app(nApp(nApp(p.oclCircularBuffer.primitive, "n1"), "n2"), lam("x", app("load", app("f", "x")))), "in")
    )
    val reduceSeq: Rule = NamedRewrite.init("ocl-reduce-seq",
      reduce --> p.oclReduceSeq.primitive
    )
    val reduceSeqUnroll: Rule = NamedRewrite.init("ocl-reduce-seq-unroll",
      rcp.reduceSeq.primitive --> p.oclReduceSeqUnroll.primitive
    )

     */
  }

  object combinatory {
    val compositionIntro: Rule = NamedRewrite.init("composition-intro",
      // only do this for functions over datatypes
      app("f" :: (("dt2": DataType) ->: ("dt3" : DataType)),
        app("g" :: (("dt1": DataType) ->: ("dt2" : DataType)), "x")) --> app("g" >> "f", "x")
    )
    val compositionElim: Rule = NamedRewrite.init("composition-elim",
      app("g" >> "f", "x") --> app("f", app("g", "x"))
    )
    val compositionAssoc1: Rule = NamedRewrite.init("composition-assoc-1",
      (("f" >> "g") >> "h") --> ("f" >> ("g" >> "h"))
    )
    val compositionAssoc2: Rule = NamedRewrite.init("composition-assoc-2",
      ("f" >> ("g" >> "h")) --> (("f" >> "g") >> "h")
    )

    val mapFusion: Rule = NamedRewrite.init("map-fusion-cnf",
      (app(map, "f") >> app(map, "g"))
        -->
      app(map, "f" >> "g")
    )
    val mapFusion2: Rule = NamedRewrite.init("map-fusion-cnf-2",
      (("h" >> app(map, "f")) >> app(map, "g"))
        -->
      ("h" >> app(map, "f" >> "g"))
    )
    val mapFission: Rule = NamedRewrite.init("map-fission-cnf",
      app(map, ("f" :: (("a": DataType) ->: ("b": DataType))) >> "g")
        -->
      (app(map, "f") >> app(map, "g"))
    )

    val reduceSeqMapFusion: Rule = NamedRewrite.init("reduce-seq-map-fusion-cnf",
      (app(map, "g") >> app(app(rcp.reduceSeq.primitive, "f"), "init"))
        -->
      app(app(rcp.reduceSeq.primitive,
        lam("acc", lam("x", app(app("f", "acc"), app("g", "x"))))),
        // lam("acc", "g" >> app("f", "acc"))),
        // lam("acc", lam("x", app("g" >> app("f", "acc"), "x")))),
        "init")
    )

    val reduceSeqMapFusion2: Rule = NamedRewrite.init("reduce-seq-map-fusion-cnf-2",
      (("h" >> app(map, "g")) >> app(app(rcp.reduceSeq.primitive, "f"), "init"))
        -->
        ("h" >> app(app(rcp.reduceSeq.primitive,
          lam("acc", lam("x", app(app("f", "acc"), app("g", "x"))))),
          // lam("acc", "g" >> app("f", "acc"))),
          // lam("acc", lam("x", app("g" >> app("f", "acc"), "x")))),
          "init"))
    )

    val reduceSeqMapFission: Rule = NamedRewrite.init("reduce-seq-map-fission-cnf",
      app(app(rcp.reduceSeq.primitive, lam("acc", lam("y",
        app(app("op", "acc"), "gy" :: ("dt": DataType))))), "init")
        -->
        lam("in", app(app(app(rcp.reduceSeq.primitive, "op"), "init"),
          app(app(map, lam("y", "gy")), "in"))),
      Seq("op" notFree "y")
    )

    def splitJoin(n: Int): Rule = NamedRewrite.init(s"split-join-cnf-$n",
      app(map, "f")
        -->
      (nApp(split, n) >> app(map, app(map, "f")) >> join)
    )
    def splitJoin1M(n: Int): Rule = NamedRewrite.init(s"split-join-1m-cnf-$n",
      app(map, app(map, "f"))
        -->
      (app(map, nApp(split, n)) >> app(map, app(map, app(map, "f"))) >> app(map, join))
    )
    def splitJoin2M(n: Int): Rule = NamedRewrite.init(s"split-join-2m-cnf-$n",
      app(map, app(map, app(map, "f")))
        -->
      (app(map, app(map, nApp(split, n))) >> app(map, app(map, app(map, app(map, "f")))) >> app(map, app(map, join)))
    )
    def splitJoin3M(n: Int): Rule = NamedRewrite.init(s"split-join-3m-cnf-$n",
      app(map, app(map, app(map, app(map, "f"))))
        -->
      (app(map, app(map, app(map, nApp(split, n)))) >> app(map, app(map, app(map, app(map, app(map, "f"))))) >> app(map, app(map, app(map, join))))
    )
    def splitJoin4M(n: Int): Rule = NamedRewrite.init(s"split-join-4m-cnf-$n",
      app(map, app(map, app(map, app(map, app(map, "f")))))
        -->
      (app(map, app(map, app(map, app(map, nApp(split, n))))) >> app(map, app(map, app(map, app(map, app(map, app(map, "f")))))) >> app(map, app(map, app(map, app(map, join)))))
    )
    def blockedReduce(n: Int): Rule = NamedRewrite.init(s"blocked-reduce-cnf-$n",
      app(app(reduce, "op" :: ("a" ->: "a" ->: ("a": Type))), "init")
        -->
      (nApp(split, n) >> app(app(rcp.reduceSeq.primitive,
        lam("acc", lam("y", app(app("op", "acc"),
          app(app(app(reduce, "op"), "init"), "y"))))),
        "init"))
    )

    val liftReduceSeq: Rule = NamedRewrite.init("lift-reduce-seq-cnf",
      app(map, app(app(rcp.reduceSeq.primitive, "op"), "init"))
        -->
      (transpose >>
      app(app(rcp.reduceSeq.primitive, lam("acc", lam("y",
        app(app(map, lam("z", app(app("op", app(fst, "z")), app(snd, "z")))),
          app(app(zip, "acc"), "y"))
      ))), app(rcp.generate.primitive, lam("i", "init"))))
    )

    // FIXME: very specific ..
    val liftReduceSeq2: Rule = NamedRewrite.init("lift-reduce-seq-cnf-2",
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

    val removeTransposePair: Rule = NamedRewrite.init("remove-transpose-pair-cnf",
      (transpose >> transpose) --> lam("x", "x")
    )
    val removeTransposePair2: Rule = NamedRewrite.init("remove-transpose-pair-cnf-2",
      (("g" >> transpose) >> transpose) --> "g"
    )
    val removeTransposePair1M: Rule = NamedRewrite.init("remove-transpose-pair-cnf-1m",
      (app(map, transpose) >> app(map, transpose)) --> lam("x", "x")
    )
    val removeTransposePair1M2: Rule = NamedRewrite.init("remove-transpose-pair-cnf-1m-2",
      (("g" >> app(map, transpose)) >> app(map, transpose)) --> "g"
    )
    val removeTransposePair2M: Rule = NamedRewrite.init("remove-transpose-pair-cnf-2m",
      (app(map, app(map, transpose)) >> app(map, app(map, transpose))) --> lam("x", "x")
    )
    val removeTransposePair2M2: Rule = NamedRewrite.init("remove-transpose-pair-cnf-2m-2",
      (("g" >> app(map, app(map, transpose))) >> app(map, app(map, transpose))) --> "g"
    )
    val compositionRightId: Rule = NamedRewrite.init("composition-right-id",
      ("f" >> lam("x", "x")) --> "f"
    )
    val compositionLeftId: Rule = NamedRewrite.init("composition-left-id",
      (lam("x", "x") >> "f") --> "f"
    )

    val mapSlideBeforeTranspose: Rule = NamedRewrite.init("map-slide-before-transpose-cnf",
      (app(map, nApp(nApp(slide, "sz"), "sp")) >> transpose)
        -->
      (transpose >> nApp(nApp(slide, "sz"), "sp") >> app(map, transpose))
    )
    val slideBeforeMapMapF: Rule = NamedRewrite.init("slide-before-map-map-f-cnf",
      (nApp(nApp(slide, "sz"), "sp") >> app(map, app(map, "f")))
        -->
      (app(map, "f") >> nApp(nApp(slide, "sz"), "sp"))
    )
    val slideBeforeMap: Rule = NamedRewrite.init("slide-before-map-cnf",
      (app(map, "f") >> nApp(nApp(slide, "sz"), "sp"))
        -->
      (nApp(nApp(slide, "sz"), "sp") >> app(map, app(map, "f")))
    )

    val splitBeforeMap: Rule = NamedRewrite.init("split-before-map-cnf",
      (app(map, "f") >> nApp(split, "n"))
        -->
      (nApp(split, "n") >> app(map, app(map, "f")))
    )
    val splitBeforeMap2: Rule = NamedRewrite.init("split-before-map-cnf-2",
      (("g" >> app(map, "f")) >> nApp(split, "n"))
        -->
      ("g" >> nApp(split, "n") >> app(map, app(map, "f")))
    )

    val transposePairAfter: Rule = NamedRewrite.init("transpose-pair-after-cnf",
      ("f" :: (t("in") ->: ((`_`: Nat)`.`((`_`: Nat)`.``_`)))) -->
      ("f" >> transpose >> transpose)
    )

    val mapMapFBeforeTranspose: Rule = NamedRewrite.init("map-map-f-before-transpose-cnf",
      (app(map, app(map, "f")) >> transpose)
        -->
      (transpose >> app(map, app(map, "f")))
    )
    val mapMapFBeforeTranspose1: Rule = NamedRewrite.init("map-map-f-before-transpose-cnf-1",
      ("g" >> app(map, app(map, "f")) >> transpose)
        -->
      ("g" >> transpose >> app(map, app(map, "f")))
    )
    val mapMapFBeforeTranspose2: Rule = NamedRewrite.init("map-map-f-before-transpose-cnf-2",
      (app(map, app(map, "f")) >> (transpose >> "g"))
        -->
      (transpose >> (app(map, app(map, "f")) >> "g"))
    )

    val transposeAroundMapMapF: Rule = NamedRewrite.init("transpose-around-map-map-f-cnf",
      app(map, app(map, "f"))
        -->
      (transpose >> app(map, app(map, "f")) >> transpose)
    )

    val transposeAroundMapMapF1M: Rule = NamedRewrite.init("transpose-around-map-map-f-1m-cnf",
      app(map, app(map, app(map, "f")))
        -->
      (app(map, transpose) >> app(map, app(map, app(map, "f"))) >> app(map, transpose))
    )

    val transposeAroundMapMapF2M: Rule = NamedRewrite.init("transpose-around-map-map-f-2m-cnf",
      app(map, app(map, app(map, app(map, "f"))))
        -->
      (app(map, app(map, transpose)) >> app(map, app(map, app(map, app(map, "f")))) >> app(map, app(map, transpose)))
    )

    val transposeAroundMapMapF3M: Rule = NamedRewrite.init("transpose-around-map-map-f-3m-cnf",
      app(map, app(map, app(map, app(map, app(map, "f")))))
        -->
      (app(map, app(map, app(map, transpose))) >> app(map, app(map, app(map, app(map, app(map, "f"))))) >> app(map, app(map, app(map, transpose))))
    )

    object vectorize {
      // TODO: `s` should be a scalar type and `m % n == 0`
      def after(n: Nat): Rule = NamedRewrite.init(s"vec-after-$n-cnf",
        ("e" :: (("m": Nat)`.`("s": DataType)))
          -->
        ("e" >> nApp(asVector, n) >> asScalar)
      )

      val asScalarAsVectorId: Rule = NamedRewrite.init("vec-as-scalar-as-vector-id-cnf",
        ((asScalar >> nApp(asVector, "n")) :: (("t": Type) ->: ("t": Type)))
          -->
        lam("in", "in")
      )
      val asScalarAsVectorId2: Rule = NamedRewrite.init("vec-as-scalar-as-vector-id-cnf-2",
        ("h" >> (asScalar >> nApp(asVector, "n")) :: (("t": Type) ->: ("t": Type)))
          -->
        "h"
      )

      // TODO: check that "f" is vectorizable / synthesize vectorized "f" ?
      /*
      val beforeMap: Rule = NamedRewrite.init("vec-before-map",
        (app(map, "f") >> nApp(asVector, "n"))
          -->
        (nApp(asVector, "n") >> app(map, "f"))
      )

       */
    }
  }
}
