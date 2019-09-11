package elevate.core

import elevate.core.strategies.basic.debug
import elevate.core.strategies.traversal._
import elevate.core.strategies.basic._
import elevate.lift.strategies.traversal._
import elevate.lift.rules._
import elevate.meta.rules.fission.FNF
import elevate.lift.strategies.tiling._
import elevate.lift.rules.movement._
import elevate.util._
import idealised.util.gen
import lift.core.DSL._
import lift.core.{Apply, DepLambda, Expr, NatIdentifier}
import lift.core.primitives._
import lift.core.types.{ArrayType, IndexType, NatKind, float, infer}
import elevate.lift._
import elevate.lift.strategies.normalForm._
import elevate.lift.rules.algorithmic._
import elevate.lift.rules.{inferLift, specialize}
import elevate.lift.strategies.tiling.{loopInterchange, loopInterchangeAtLevel}

import scala.language.implicitConversions

class algorithmic extends idealised.util.Tests {

  implicit def rewriteResultToExpr(r: RewriteResult[Expr]): Expr = r.get

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

    val mapReduce = LCNF(
      DepLambda[NatKind](M, DepLambda[NatKind](N,
      fun(ArrayType(M, ArrayType(N, float)))(i =>
        map(reduce(fun(x => fun(a => x + a)))(l(0.0f) :: float)) $ i)))).get

    val reduceMap =
      DepLambda[NatKind](M, DepLambda[NatKind](N,
        fun(ArrayType(M, ArrayType(N, float)))(i =>
          reduce(fun((y, acc) =>
            map(addTuple) $ zip(y, acc)))(generate(fun(IndexType(M) ->: float)(_ => l(0.0f)))) $ transpose(i))))

    val rewrite = body(body(body(liftReduce))).apply(infer(mapReduce)).get

    infer(mapReduce)

    exprToDot("/home/bastian/development/rewriting/dot", "input", infer(mapReduce), dotPrinter(_))
    val typedGold = infer(reduceMap)
    val typedRewrite = infer(rewrite)

    assert(typedRewrite == typedGold)
  }

  test("MM to MM-LoopMKN") {
    val M = NatIdentifier("M")
    val N = NatIdentifier("N")
    val K = NatIdentifier("K")

    val mm = infer(LCNF(DepLambda[NatKind](M, DepLambda[NatKind](N, DepLambda[NatKind](K,
      fun(ArrayType(M, ArrayType(K, float)))(a =>
        fun(ArrayType(K, ArrayType(N, float)))(b =>
          map(fun(ak =>
            map(fun(bk =>
              (reduce(fun((y, acc) => acc + (y._1 * y._2)), l(0.0f))) $
                zip(ak, bk))) $ transpose(b) )) $ a)))))).get)

    def goldMKN(reduceFun: Expr): Expr = {
      DepLambda[NatKind](M, DepLambda[NatKind](N, DepLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            map(fun(ak =>
              reduce(
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
      fun((y, acc) => // y :: (float, N.float); acc :: N.float
        mapSeq(fun(t => fst(t) + snd(t))) $
          zip(acc,
            mapSeq(fun(bs => bs * fst(y))) $ snd(y))
      )
    )

    val goldMKNAlternative = goldMKN(
     fun((aBN, acc) => { // akB :: (float, N.float); acc :: N.float
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

    val loopMKN = (oncetd(liftReduce) `;` oncetd(removeTransposePair)).apply(mm).get
    val typedRewrite = infer(loopMKN)

    // todo something's wrong with the way of comparing typed expressions
    //assert(typedRewrite == typedGold)

    val gold = normalize(untype)(typedGold).get
    val rewrite = normalize(untype)(typedGold).get


  }

  test("MM-LoopMKN to MM-LoopKMN") {
    val M = NatIdentifier("M")
    val N = NatIdentifier("N")
    val K = NatIdentifier("K")

    val mmMKN: Expr = {
      DepLambda[NatKind](M, DepLambda[NatKind](N, DepLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            map(fun(ak =>
              reduce(
                fun((y, acc) => { // akB :: (float, N.float); acc :: N.float
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
      DepLambda[NatKind](M, DepLambda[NatKind](N, DepLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            reduce(
              fun((y2, acc2) => // y2 :: (M.float, N.float); acc2 :: M.N.float
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
    val op = fun((y, acc) => { // akB :: (float, N.float); acc :: N.float
      map(fun(t => fst(t) + (fst(snd(t)) * snd(snd(t))))) $
        /* N.(float, (float, float))*/
        zip(acc,
          map(fun(bs => pair(bs, fst(y)))) $ snd(y)/*:N.float*/)
    })

    // this one is constructed more similar to what the rewrite rules will create
    val goldKMNAlternative =
      DepLambda[NatKind](M, DepLambda[NatKind](N, DepLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            reduce(
              fun((y, acc) => // y :: (M.float, N.float); acc :: M.N.float
                map(fun(x => // x :: M.((float, N.float), N.float)
                  Apply(Apply(op, fst(x)), snd(x)))) $
                  // M.((float, N.float), N.float)
                  zip(map(fun(t => pair(t,snd(y)))) $ fst(y), acc)),
              // generate zeros :: M.N.float
              generate(fun(IndexType(M) ->: ArrayType(N, float))(_ =>
                generate(fun(IndexType(N) ->: float)(_ => l(0.0f) ))))
            ) $
              zip(transpose(a), b) // :: K.(M.float, N.float)
          ))
      )))

    // unfortunately, the order of zip arguments is important
    val goldKMNAlternative2 =
      DepLambda[NatKind](M, DepLambda[NatKind](N, DepLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            reduce(
              fun((y, acc) => // y :: (M.float, N.float); acc :: M.N.float
                map(fun(x => // x :: M.((float, N.float), N.float)
                  Apply(Apply(op, fst(x)), snd(x)))) $
                  // M.((N.float, float), N.float)
                  zip(map(fun(t => pair(t,fst(y)))) $ snd(y), acc)),
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
    assert(untypeExpr(goldKMNAlternative2) == untypeExpr(loopKMN))

    val goldKMNAlternative2LowLevel =
      DepLambda[NatKind](M, DepLambda[NatKind](N, DepLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, float)))(a =>
          fun(ArrayType(K, ArrayType(N, float)))(b =>
            reduceSeq(
              fun((y, acc) => // y :: (M.float, N.float); acc :: M.N.float
                mapSeq(fun(x => // x :: M.((float, N.float), N.float)
                  Apply(Apply(fun((y, acc) => { // akB :: (float, N.float); acc :: N.float
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

    println(gen.CProgram(infer(goldKMNAlternative2LowLevel)))
  }

  test("mm tile + reorder") {
    val M = NatIdentifier("M")
    val N = NatIdentifier("N")
    val K = NatIdentifier("K")

    val mm =
      LCNF(DepLambda[NatKind](M, DepLambda[NatKind](N, DepLambda[NatKind](K,
      fun(ArrayType(M, ArrayType(K, float)))(a =>
        fun(ArrayType(K, ArrayType(N, float)))(b =>
          map(fun(ak =>
            map(fun(bk =>
              (reduce(fun((y, acc) => acc + (y._1 * y._2)), l(0.0f))) $
                zip(ak, bk))) $ transpose(b) )) $ a)))))).get

    val typedMM = infer(mm)

    // sanity check
    assert(untypeExpr(mm) == untypeExpr(typedMM))

    val tile = body(body(body(body(body(tileND(2)(32)))))) `;` LCNF

    val untyped = tile.apply(mm).get
    val typed = tile.apply(typedMM).get

    val typedUntyped = infer(untyped)
    infer(typed)

    // these should be correct, it's just that the mapAcceptorTranslation for split is not defined yet
    val lower: Strategy[Lift] = LCNF `;` CNF `;` normalize(specialize.mapSeq <+ specialize.reduceSeq) `;` BENF
    println(gen.CProgram(infer(lower(typedUntyped).get)))
    assert(untypeExpr(untyped) == untypeExpr(typed))

    /// TILE + REORDER

    val tileReorder = body(body(body(body(body(tileNDList(List(16,32))))))) `;` LCNF `;`
      inferLift `;` oncetd(liftReduce) `;` inferLift `;` oncetd(liftReduce)

    val reorder = tileReorder(mm).get

    val fixed = infer(reorder)

    println(gen.CProgram(lower(fixed).get))
  }

  test("map map zip") {
      val M = NatIdentifier("M")
      val N = NatIdentifier("N")
      val K = NatIdentifier("K")

      val mm =
        LCNF(DepLambda[NatKind](M, DepLambda[NatKind](N, DepLambda[NatKind](K,
          fun(ArrayType(M, ArrayType(K, float)))(a =>
            fun(ArrayType(K, ArrayType(N, float)))(b =>
              map(fun(ak =>
                map(fun(bk =>
                  (reduce(fun((y, acc) => acc + (y._1 * y._2)), l(0.0f))) $
                    zip(ak, bk))) $ transpose(b) )) $ a)))))).get

      val rnf = (RNF `;` BENF)(mm).get

      exprToDot("/home/bastian/development/rewriting/dot", "untyped", mm, dotPrinter(_))
      exprToDot("/home/bastian/development/rewriting/dot", "typed", rnf, dotPrinter(_))

    val lower: Strategy[Lift] = normalize(specialize.mapSeq <+ specialize.reduceSeq)
    println(gen.CProgram(infer(lower(rnf).get)))

   }

  test("tile mm") {

    // loop ordering: M -> N -> K
    val mm =
      nFun((m, n, k) =>
        fun((m`.`k`.`float) ->: (k`.`n`.`float) ->: (m`.`n`.`float))
        ((a, b) =>
          map(fun(ak =>
            map(fun(bk =>
              (reduceSeq(fun((y, acc) => acc + (y._1 * y._2)), l(0.0f))) $
                zip(ak, bk))) $ transpose(b) )) $ a
        )
      )

    val M = NatIdentifier("M")
    val N = NatIdentifier("N")
    val K = NatIdentifier("K")

    // loop ordering: M -> K -> N
    val mmLoopMKN =
      DepLambda[NatKind](M, DepLambda[NatKind](N, DepLambda[NatKind](K,
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
      DepLambda[NatKind](M, DepLambda[NatKind](N, DepLambda[NatKind](K,
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
      DepLambda[NatKind](M, DepLambda[NatKind](N, DepLambda[NatKind](K,
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
      DepLambda[NatKind](M, DepLambda[NatKind](N, DepLambda[NatKind](K,
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
    exprToDot("/home/bastian/development/rewriting/dot", "input", tiled, dotPrinter(_))
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
