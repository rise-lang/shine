package elevate.core

import elevate.core.strategies.traversal._
import elevate.core.strategies.basic._
import elevate.rise.rules.traversal._
import elevate.rise.strategies.tiling._
import elevate.util._
import util.gen
import lift.core.DSL._
import lift.core.dotPrinter._
import lift.core.{Expr, NatIdentifier}
import lift.core.types.{ArrayType, IndexType, NatKind, float, infer}
import elevate.rise._
import elevate.rise.strategies.normalForm._
import elevate.rise.strategies.traversal._
import elevate.rise.rules.algorithmic._
import elevate.rise.rules.movement.liftReduce
import elevate.rise.rules.{inferRise, lowering}
import elevate.rise.strategies.tiling.{loopInterchange, loopInterchangeAtLevel}

class algorithmic extends test_util.Tests {

  /// LOOP INTERCHANGE

  test("simple loop interchange") {
    assert(betaEtaEquals(
      body(body(loopInterchange))(λ(i => λ(f => **!(f) $ i))),
      λ(i => λ(f => (T o **(f) o T) $ i))
    ))
  }

  test("interchange innermost two loops in loop nest of depth 3") {
    val input = λ(i => λ(f => ***!(f) $ i))
    val gold = λ(i => λ(f => (*(T) o ***(f) o *(T)) $ i))

    assert(betaEtaEquals(
      body(body(loopInterchangeAtLevel(1)))(input),
      gold
    ))

    assert(betaEtaEquals(
      body(body(fmap(loopInterchange) `;` LCNF `;` RNF))(input),
      gold
    ))
  }

  test("lift reduce") {
    def M = NatIdentifier("M")
    def N = NatIdentifier("N")

    val addTuple = fun(x => fst(x) + snd(x))

    val mapReduce = LCNF(infer(
      depLambda[NatKind](M, depLambda[NatKind](N,
      fun(ArrayType(M, ArrayType(N, float)))(i =>
        map(reduce(fun(x => fun(a => x + a)))(l(0.0f))) $ i))))).get

    exprToDot("test", mapReduce)

    val reduceMap =
      depLambda[NatKind](M, depLambda[NatKind](N,
        fun(ArrayType(M, ArrayType(N, float)))(i =>
          reduce(fun((acc, y) =>
            map(addTuple) $ zip(acc, y)))(generate(fun(IndexType(M) ->: float)(_ => l(0.0f)))) $ transpose(i))))

    val rewrite = body(body(liftReduce)).apply(mapReduce).get

    infer(mapReduce)

    val typedGold = infer(reduceMap)
    val typedRewrite = infer(rewrite)

    assert(typedRewrite == typedGold)
  }

  test("MM to MM-LoopMKN") {
    val M = NatIdentifier("M")
    val N = NatIdentifier("N")
    val K = NatIdentifier("K")

    val mm = LCNF(infer(depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
      fun(ArrayType(M, ArrayType(K, float)))(a =>
        fun(ArrayType(K, ArrayType(N, float)))(b =>
          map(fun(ak =>
            map(fun(bk =>
              (reduceSeq(fun((acc, y) => acc + (y._1 * y._2)), l(0.0f))) $
                zip(ak, bk))) $ transpose(b) )) $ a))))))).get

    def goldMKN(reduceFun: Expr): Expr = {
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            map(fun(ak =>
              reduceSeq(
                reduceFun,
                generate(fun(IndexType(N) ->: float)(_ => l(0.0f)))
              ) $
                zip(ak, b))
            ) $ a
          )
        )
      )))
    }

    val goldMKNVersion1 = goldMKN(
      fun((acc, y) => // y :: (float, N.float); acc :: N.float
        mapSeq(fun(t => fst(t) + snd(t))) $
          zip(acc,
            mapSeq(fun(bs => bs * fst(y))) $ snd(y))
      )
    )

    val goldMKNAlternative = goldMKN(
     fun((acc, aBN) => { // akB :: (float, N.float); acc :: N.float
       val BN = snd(aBN)
       val as = fst(aBN)
       map(fun(t => fst(t) + (fst(snd(t)) * snd(snd(t))))) $
         /* N.(float, (float, float))*/
         zip(acc,
           map(fun(bs => pair(bs, as))) $ BN/*:N.float*/)
     })
    )

    infer(goldMKNVersion1)
    val typedGold = infer(goldMKNAlternative)

    val loopMKN = (oncetd(liftReduce) `;` LCNF `;` oncetd(removeTransposePair)).apply(mm).get
    val typedRewrite = infer(loopMKN)

    // todo something's wrong with the way of comparing typed expressions
    //assert(typedRewrite == typedGold)
    //val gold = normalize(untype)(typedGold).get
    //val rewrite = normalize(untype)(typedGold).get


  }

  // FIXME
  ignore("MM-LoopMKN to MM-LoopKMN") {
    val M = NatIdentifier("M")
    val N = NatIdentifier("N")
    val K = NatIdentifier("K")

    val mmMKN: Expr = {
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            map(fun(ak =>
              reduceSeq(
                fun((acc, y) => { // akB :: (float, N.float); acc :: N.float
                  map(fun(t => fst(t) + (fst(snd(t)) * snd(snd(t))))) $
                    /* N.(float, (float, float))*/
                    zip(acc,
                      map(fun(bs => pair(bs, fst(y)))) $ snd(y)/*:N.float*/)
                }),
                generate(fun(IndexType(N) ->: float)(_ => l(0.0f)))
              ) $
                zip(ak, b))
            ) $ a
          )
        )
      )))
    }

    // this one is handwritten and uses zip2d
    val goldKMN =
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            reduce(
              fun((acc2, y2) => // y2 :: (M.float, N.float); acc2 :: M.N.float
                map(fun(x => // x :: M.(float, N.float)
                  map(fun(bnAcc => // bnAcc :: N.(float, float)
                    mul(fst(x),fst(bnAcc)) + snd(bnAcc))) $
                    zip(snd(y2), snd(x)))) $
                  zip(fst(y2), acc2)),
              // generate zeros :: M.N.float
              generate(fun(IndexType(M) ->: ArrayType(N, float))(_ =>
                generate(fun(IndexType(N) ->: float)(_ => l(0.0f) ))))
            ) $
              zip(transpose(a), b) // :: K.(M.float, N.float)
          ))
      )))

    // taken from input
    val op = fun((acc, y) => { // akB :: (float, N.float); acc :: N.float
      map(fun(t => fst(t) + (fst(snd(t)) * snd(snd(t))))) $
        /* N.(float, (float, float))*/
        zip(acc,
          map(fun(bs => pair(bs, fst(y)))) $ snd(y)/*:N.float*/)
    })

    // this one is constructed more similar to what the rewrite rules will create
    val goldKMNAlternative =
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            reduceSeq(
              fun((acc, y) => // y :: (M.float, N.float); acc :: M.N.float
                map(fun(x => // x :: M.((float, N.float), N.float)
                  app(app(op, fst(x)), snd(x)))) $
                  // M.((float, N.float), N.float)
                  zip(acc, map(fun(t => pair(t,snd(y)))) $ fst(y))),
              // generate zeros :: M.N.float
              generate(fun(IndexType(M) ->: ArrayType(N, float))(_ =>
                generate(fun(IndexType(N) ->: float)(_ => l(0.0f) ))))
            ) $
              zip(transpose(a), b) // :: K.(M.float, N.float)
          ))
      )))

    // unfortunately, the order of zip arguments is important
    val goldKMNAlternative2 =
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            reduceSeq(
              fun((acc, y) => // y :: (M.float, N.float); acc :: M.N.float
                map(fun(x => // x :: M.((float, N.float), N.float)
                  app(app(op, fst(x)), snd(x)))) $
                  // M.((N.float, float), N.float)
                  zip(acc, map(fun(t => pair(t,fst(y)))) $ snd(y))),
              // generate zeros :: M.N.float
              generate(fun(IndexType(M) ->: ArrayType(N, float))(_ =>
                generate(fun(IndexType(N) ->: float)(_ => l(0.0f) ))))
            ) $
              zip(b,transpose(a)) // :: K.(N.float, M.float)
          ))
      )))

    infer(goldKMNAlternative)
    infer(goldKMNAlternative2)

    val loopKMN = (oncetd(liftReduce)).apply(infer(mmMKN)).get

    infer(loopKMN)
    assert(goldKMNAlternative2 == loopKMN)

    val goldKMNAlternative2LowLevel =
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            reduceSeq(
              fun((acc, y) => // y :: (M.float, N.float); acc :: M.N.float
                mapSeq(fun(x => // x :: M.((float, N.float), N.float)
                  app(app(fun((y, acc) => { // akB :: (float, N.float); acc :: N.float
                    mapSeq(fun(t => fst(t) + (fst(snd(t)) * snd(snd(t))))) $
                      /* N.(float, (float, float))*/
                      zip(acc,
                        map(fun(bs => pair(bs, fst(y)))) $ snd(y)/*:N.float*/)
                  }), fst(x)), snd(x)))) $
                  // M.((N.float, float), N.float)
                  zip(map(fun(t => pair(t,fst(y)))) $ snd(y), acc)),
              // generate zeros :: M.N.float
              generate(fun(IndexType(M) ->: ArrayType(N, float))(_ =>
                generate(fun(IndexType(N) ->: float)(_ => l(0.0f) ))))
            ) $
              zip(b,transpose(a)) // :: K.(N.float, M.float)
          ))
      )))

    println(gen.CProgram(infer(goldKMNAlternative2LowLevel)).code)
  }

  // FIXME
  ignore("mm tile + reorder") {
    val M = NatIdentifier("M")
    val N = NatIdentifier("N")
    val K = NatIdentifier("K")

    val mm =
      LCNF(depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
      fun(ArrayType(M, ArrayType(K, float)))(a =>
        fun(ArrayType(K, ArrayType(N, float)))(b =>
          map(fun(ak =>
            map(fun(bk =>
              (reduceSeq(fun((acc, y) => acc + (y._1 * y._2)), l(0.0f))) $
                zip(ak, bk))) $ transpose(b) )) $ a)))))).get

    val typedMM = infer(mm)

    // sanity check
    assert(mm == typedMM)

    val tile = body(body(body(body(body(tileND(2)(32)))))) `;` LCNF

    val untyped = tile.apply(mm).get
    val typed = tile.apply(typedMM).get

    val typedUntyped = infer(untyped)
    infer(typed)

    // these should be correct, it's just that the mapAcceptorTranslation for split is not defined yet
    val lower: Strategy[Rise] = LCNF `;` CNF `;` normalize.apply(lowering.mapSeq <+ lowering.reduceSeq) `;` BENF
    println(gen.CProgram(infer(lower(typedUntyped).get)).code)
    assert(untyped == typed)

    /// TILE + REORDER

    val tileReorder = body(body(body(body(body(tileNDList(List(16,32))))))) `;` LCNF `;`
      inferRise `;` oncetd(liftReduce) `;` inferRise `;` oncetd(liftReduce)

    val reorder = tileReorder(mm).get

    val fixed = infer(reorder)

    println(gen.CProgram(lower(fixed).get).code)
  }

  // FIXME
  ignore("map map zip") {
      val M = NatIdentifier("M")
      val N = NatIdentifier("N")
      val K = NatIdentifier("K")

      val mm =
        LCNF(depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
          fun(ArrayType(M, ArrayType(K, float)))(a =>
            fun(ArrayType(K, ArrayType(N, float)))(b =>
              map(fun(ak =>
                map(fun(bk =>
                  (reduceSeq(fun((acc, y) => acc + (y._1 * y._2)), l(0.0f))) $
                    zip(ak, bk))) $ transpose(b) )) $ a)))))).get

      val rnf = (RNF `;` BENF)(mm).get

    val lower: Strategy[Rise] = normalize.apply(lowering.mapSeq <+ lowering.reduceSeq)
    println(gen.CProgram(infer(lower(rnf).get)).code)

   }

  test("tile mm") {

    // loop ordering: M -> N -> K
    val mm =
      nFun((m, n, k) =>
        fun((m`.`k`.`float) ->: (k`.`n`.`float) ->: (m`.`n`.`float))
        ((a, b) =>
          map(fun(ak =>
            map(fun(bk =>
              (reduceSeq(fun((acc, y) => acc + (y._1 * y._2)), l(0.0f))) $
                zip(ak, bk))) $ transpose(b) )) $ a
        )
      )

    val M = NatIdentifier("M")
    val N = NatIdentifier("N")
    val K = NatIdentifier("K")

    // loop ordering: M -> K -> N
    val mmLoopMKN =
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            mapSeq(fun(ak =>
              reduceSeq(
                fun((y, acc) => // y :: (float, N.float); acc :: N.float
                  mapSeq(fun(t => fst(t) + snd(t))) $
                    zip(acc,
                      mapSeq(fun(bs => bs * fst(y))) $ snd(y))
                ),
                generate(fun(IndexType(N) ->: float)(_ => l(0.0f)))
              ) $
                zip(ak, b))
            ) $ a
          )
        )
      )))

    // loop ordering: M -> K -> N
    val mmLoopMKNFused =
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            mapSeq(fun(ak =>
              reduceSeq(
                fun((y, acc) => // y :: (float, N.float); acc :: N.float
                  mapSeq(fun(bs => (fst(bs) * fst(y)) + snd(bs))) $ zip(snd(y), acc)
                ),
                generate(fun(IndexType(N) ->: float)(_ => l(0.0f)))
              ) $
                zip(ak, b))
            ) $ a
          )
        )
      )))

    // loop ordering: K -> M -> N
    val mmOuterProduct =
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            reduceSeq(
              fun((y, acc) => // y :: (M.float, N.float); acc :: M.N.float
                // 3. add accumulator and computed outer product
                (mapSeq(mapSeq(fun(tuple => fst(tuple) + snd(tuple)))) o
                  // 2. zip2D accumulator + computed outer product
                  map(fun(t => zip(fst(t), snd(t))))) $ zip(acc, // here the map-acceptor translation works
                  // 1. compute one outer product
                  mapSeq(fun(am => mapSeq(fun(bn => mul(am,bn))) $ snd(y))) $ fst(y))),
              // generate zeros :: M.N.float
              generate(fun(IndexType(M) ->: ArrayType(N, float))(_ =>
                generate(fun(IndexType(N) ->: float)(_ => l(0.0f) ))))
            ) $
              zip(transpose(a), b) // :: K.(M.float, N.float)
          ))
      )))

    // loop ordering: K -> M -> N
    val mmOuterProductFusedComputation =
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            reduceSeq(
              fun((y, acc) => // y :: (M.float, N.float); acc :: M.N.float
                mapSeq(fun(amAccn => // amAccn :: M.(float, N.float)
                  mapSeq(fun(bnAcc => // bnAcc :: N.(float, float)
                    mul(fst(amAccn),fst(bnAcc)) + snd(bnAcc))) $
                    zip(snd(y), snd(amAccn)))) $
                  zip(fst(y), acc)),
              // generate zeros :: M.N.float
              generate(fun(IndexType(M) ->: ArrayType(N, float))(_ =>
                generate(fun(IndexType(N) ->: float)(_ => l(0.0f) ))))
            ) $
              zip(transpose(a), b) // :: K.(M.float, N.float)
          ))
      )))

    val tiled = oncetd(tileND(2)(16)).apply(mm)
    infer(tiled)
  }

  test("reduce rows") {
    val addT = fun(x => fst(x) + snd(x))

    val test = nFun(n => nFun(m => fun(ArrayType(n, ArrayType(m, float)))(i =>
      reduceSeq(fun((y, acc) =>
        map(addT) $ zip(y)(acc)),
        generate(fun(IndexType(m) ->: float)(_ => l(0.0f)))) $ i)))

    infer(test)
  }

  test("dot and outer product") {
    def xsT(N : NatIdentifier) = ArrayType(N, float)
    def ysT(N : NatIdentifier) = ArrayType(N, float)

    val mulT = fun(x => fst(x) * snd(x))
    val add = fun(x => fun(a => x + a))

    val dot = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
      (reduce(add)(l(0.0f)) o map(mulT)) $ zip(xs)(ys)
    )))

    val outer = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
      map(fun(a => map(fun(b => add(a,b))) $ ys )) $ xs
    )))

    infer(dot)
    infer(outer)
  }
}
