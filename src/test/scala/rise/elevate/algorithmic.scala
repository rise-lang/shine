package rise.elevate

import _root_.util.gen
import elevate.core.Strategy
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering
import rise.elevate.rules.movement.liftReduce
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.traversal._
import rise.elevate.util._

class algorithmic extends test_util.Tests {

  // - Notation -
  // x >> y: piping operator, x then y
  // *f: map(f)
  // T: transpose
  // S: slide/split
  // J: join

  def loopInterchange = rise.elevate.strategies.tiling.loopInterchange(default.RiseTraversable)
  def loopInterchangeAtLevel = rise.elevate.strategies.tiling.loopInterchangeAtLevel(default.RiseTraversable)
  def tileND = rise.elevate.strategies.tiling.tileND(default.RiseTraversable)
  def tileNDList = rise.elevate.strategies.tiling.tileNDList(default.RiseTraversable)

  def DFNF = rise.elevate.strategies.normalForm.DFNF()(default.RiseTraversable)
  def RNF = rise.elevate.strategies.normalForm.RNF()(default.RiseTraversable)
  def CNF = rise.elevate.strategies.normalForm.CNF()(default.RiseTraversable)
  def BENF = rise.elevate.strategies.normalForm.BENF()(default.RiseTraversable)

  // Loop Interchange

  test("simple loop interchange") {
    assert(betaEtaEquals(
      body(body(loopInterchange))(λ(i => λ(f => **!(f) $ i))).get,
      λ(i => λ(f => (T o **(f) o T) $ i))
    ))
  }

  test("interchange innermost two loops in loop nest of depth 3") {
    val input = λ(i => λ(f => ***!(f) $ i))
    val gold = λ(i => λ(f => (*(T) o ***(f) o *(T)) $ i))

    assert(betaEtaEquals(
      body(body(loopInterchangeAtLevel(1)))(input).get,
      gold
    ))

    assert(betaEtaEquals(
      body(body(fmap(loopInterchange) `;` DFNF `;` RNF))(input).get,
      gold
    ))
  }

  // Swap Nesting of map and reduce

  test("lift reduce") {
    val M = NatIdentifier("M")
    val N = NatIdentifier("N")

    val addTuple = fun(x => fst(x) + snd(x))

    val mapReduce = depLambda(NatKind, M, depLambda(NatKind, N,
      fun(ArrayType(M, ArrayType(N, f32)))(i =>
        map(reduce(fun(x => fun(a => x + a)))(lf32(0.0f))) $ i)))

    val reduceMap: Rise =
      depLambda(NatKind, M, depLambda(NatKind, N,
        fun(ArrayType(M, ArrayType(N, f32)))(i =>
          reduce(fun((acc, y) =>
            map(addTuple) $ zip(acc)(y)))(generate(fun(IndexType(M) ->: f32)(_ => lf32(0.0f)))) $ transpose(i))))

    val rewrite = body(body(body(function(liftReduce))))(DFNF(mapReduce).get).get

    val typedGold = DFNF(reduceMap)
    val typedRewrite = DFNF(rewrite)

    assert(typedRewrite.get =~= typedGold.get)
  }


  // Tests and Expressions related to loop reordering in Matrix Multiplication

  test("MM to MM-LoopMKN") {
    val M = NatIdentifier("M")
    val N = NatIdentifier("N")
    val K = NatIdentifier("K")

    val mm = depLambda(NatKind, M, depLambda(NatKind, N, depLambda(NatKind, K,
      fun(ArrayType(M, ArrayType(K, f32)))(a =>
        fun(ArrayType(K, ArrayType(N, f32)))(b =>
          a |> map(fun(ak =>
            transpose(b) |> map(fun(bk =>
              zip(ak)(bk) |>
                reduceSeq(fun((acc, y) => acc + (y.`1` * y.`2`)))(
                  lf32(0.0f)))))))))))

    def goldMKN(reduceFun: ToBeTyped[Rise]): ToBeTyped[Rise] = {
      depLambda(NatKind, M, depLambda(NatKind, N, depLambda(NatKind, K,
        fun(ArrayType(M, ArrayType(K, f32)))(a =>
          fun(ArrayType(K, ArrayType(N, f32)))(b =>
            a |> map(fun(ak =>
              zip(ak)(b) |> reduceSeq(reduceFun)(
                generate(fun(IndexType(N) ->: f32)(_ => lf32(0.0f)))))))))))
    }

    val goldMKNVersion1 = goldMKN(
      fun((acc, y) => // y :: (f32, N.f32); acc :: N.f32
        mapSeq(fun(t => fst(t) + snd(t))) $
          zip(acc)(
            mapSeq(fun(bs => bs * fst(y))) $ snd(y))
      )
    )

    val goldMKNAlternative = goldMKN(
     fun((acc, aBN) => { // akB :: (f32, N.f32); acc :: N.f32
       val BN = snd(aBN)
       val as = fst(aBN)
       map(fun(t => fst(t) + (fst(snd(t)) * snd(snd(t))))) $
         /* N.(f32, (f32, f32))*/
         zip(acc)(
           map(fun(bs => makePair(bs)(as))) $ BN/*:N.f32*/)
     })
    )

    goldMKNVersion1.toExpr
    val typedGold = goldMKNAlternative.toExpr
    val loopMKN = (topDown(liftReduce) `;` DFNF `;` topDown(removeTransposePair)).apply(mm).get

    assert(loopMKN =~= typedGold)
  }

  // This one just serves as documentation for different mm-rise-expressions
  ignore("MM-LoopMKN to MM-LoopKMN") {
    val M = NatIdentifier("M")
    val N = NatIdentifier("N")
    val K = NatIdentifier("K")

    val mmMKN = {
      depLambda(NatKind, M, depLambda(NatKind, N, depLambda(NatKind, K,
        fun(ArrayType(M, ArrayType(K, f32)))(a =>
          fun(ArrayType(K, ArrayType(N, f32)))(b =>
            map(fun(ak =>
              reduceSeq(
                fun((acc, y) => { // akB :: (f32, N.f32); acc :: N.f32
                  map(fun(t => fst(t) + (fst(snd(t)) * snd(snd(t))))) $
                    /* N.(f32, (f32, f32))*/
                    zip(acc)(
                      map(fun(bs => makePair(bs)(fst(y)))) $ snd(y)/*:N.f32*/)
                }))(
                generate(fun(IndexType(N) ->: f32)(_ => lf32(0.0f)))
              ) $
                zip(ak)(b))
            ) $ a
          )
        )
      )))
    }

    // this one is handwritten and uses zip2d
    /*
    val goldKMN =
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, f32)))(a =>
          fun(ArrayType(K, ArrayType(N, f32)))(b =>
            reduce(
              fun((acc2, y2) => // y2 :: (M.f32, N.f32); acc2 :: M.N.f32
                map(fun(x => // x :: M.(f32, N.f32)
                  map(fun(bnAcc => // bnAcc :: N.(f32, f32)
                    mul(fst(x),fst(bnAcc)) + snd(bnAcc))) $
                    zip(snd(y2), snd(x)))) $
                  zip(fst(y2), acc2)),
              // generate zeros :: M.N.f32
              generate(fun(IndexType(M) ->: ArrayType(N, f32))(_ =>
                generate(fun(IndexType(N) ->: f32)(_ => l(0.0f) ))))
            ) $
              zip(transpose(a), b) // :: K.(M.f32, N.f32)
          ))
      )))
    */

    // taken from input
    val op = fun((acc, y) => { // akB :: (f32, N.f32); acc :: N.f32
      map(fun(t => fst(t) + (fst(snd(t)) * snd(snd(t))))) $
        /* N.(f32, (f32, f32))*/
        zip(acc)(
          map(fun(bs => makePair(bs)(fst(y)))) $ snd(y)/*:N.f32*/)
    })

    // this one is constructed more similar to what the rewrite rules will create
    val goldKMNAlternative =
      depLambda(NatKind, M, depLambda(NatKind, N, depLambda(NatKind, K,
        fun(ArrayType(M, ArrayType(K, f32)))(a =>
          fun(ArrayType(K, ArrayType(N, f32)))(b =>
            reduceSeq(
              fun((acc, y) => // y :: (M.f32, N.f32); acc :: M.N.f32
                map(fun(x => // x :: M.((f32, N.f32), N.f32)
                  app(app(op, fst(x)), snd(x)))) $
                  // M.((f32, N.f32), N.f32)
                  zip(acc)(map(fun(t => makePair(t)(snd(y)))) $ fst(y))))(
              // generate zeros :: M.N.f32
              generate(fun(IndexType(M) ->: ArrayType(N, f32))(_ =>
                generate(fun(IndexType(N) ->: f32)(_ => lf32(0.0f) ))))
            ) $
              zip(transpose(a))(b) // :: K.(M.f32, N.f32)
          ))
      )))

    // unfortunately, the order of zip arguments is important
    val goldKMNAlternative2 =
      depLambda(NatKind, M, depLambda(NatKind, N, depLambda(NatKind, K,
        fun(ArrayType(M, ArrayType(K, f32)))(a =>
          fun(ArrayType(K, ArrayType(N, f32)))(b =>
            reduceSeq(
              fun((acc, y) => // y :: (M.f32, N.f32); acc :: M.N.f32
                map(fun(x => // x :: M.((f32, N.f32), N.f32)
                  app(app(op, fst(x)), snd(x)))) $
                  // M.((N.f32, f32), N.f32)
                  zip(acc)(map(fun(t => makePair(t)(fst(y)))) $ snd(y))))(
              // generate zeros :: M.N.f32
              generate(fun(IndexType(M) ->: ArrayType(N, f32))(_ =>
                generate(fun(IndexType(N) ->: f32)(_ => lf32(0.0f) ))))
            ) $
              zip(b)(transpose(a)) // :: K.(N.f32, M.f32)
          ))
      )))

    goldKMNAlternative.toExpr
    goldKMNAlternative2.toExpr

    val loopKMN = topDown(liftReduce).apply(mmMKN.toExpr).get

    assert(goldKMNAlternative2.toExpr =~= loopKMN)

    /*
    val goldKMNAlternative2LowLevel =
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, f32)))(a =>
          fun(ArrayType(K, ArrayType(N, f32)))(b =>
            reduceSeq(
              fun((acc, y) => // y :: (M.f32, N.f32); acc :: M.N.f32
                mapSeq(fun(x => // x :: M.((f32, N.f32), N.f32)
                  app(app(fun((y, acc) => { // akB :: (f32, N.f32); acc :: N.f32
                    mapSeq(fun(t => fst(t) + (fst(snd(t)) * snd(snd(t))))) $
                      /* N.(f32, (f32, f32))*/
                      zip(acc,
                        map(fun(bs => pair(bs, fst(y)))) $ snd(y)/*:N.f32*/)
                  }), fst(x)), snd(x)))) $
                  // M.((N.f32, f32), N.f32)
                  zip(map(fun(t => pair(t,fst(y)))) $ snd(y), acc)),
              // generate zeros :: M.N.f32
              generate(fun(IndexType(M) ->: ArrayType(N, f32))(_ =>
                generate(fun(IndexType(N) ->: f32)(_ => l(0.0f) ))))
            ) $
              zip(b,transpose(a)) // :: K.(N.f32, M.f32)
          ))
      )))
    */
  }

  // todo remove once PLDI-TVM tests are in
  ignore("mm tile + reorder") {
    val M = NatIdentifier("M")
    val N = NatIdentifier("N")
    val K = NatIdentifier("K")

    val mm =
      DFNF(depLambda(NatKind, M, depLambda(NatKind, N, depLambda(NatKind, K,
      fun(ArrayType(M, ArrayType(K, f32)))(a =>
        fun(ArrayType(K, ArrayType(N, f32)))(b =>
          map(fun(ak =>
            map(fun(bk =>
              (reduceSeq(fun((acc, y) => acc + (y.`1` * y.`2`)))(lf32(0.0f))) $
                zip(ak)(bk))) $ transpose(b) )) $ a)))))).get

    val tile = body(body(body(body(body(tileND(2)(32)))))) `;` DFNF

    val typed = tile.apply(mm).get

    // these should be correct, it's just that the mapAcceptorTranslation for split is not defined yet
    val lower: Strategy[Rise] = DFNF `;` CNF `;` normalize.apply(lowering.mapSeq <+ lowering.reduceSeq) `;` BENF
    logger.debug(gen.c.function.asStringFromExpr(lower(typed).get))

    /// TILE + REORDER

    val tileReorder = body(body(body(body(body(tileNDList(List(16,32))))))) `;` DFNF `;`
      topDown(liftReduce) `;` topDown(liftReduce)

    val reorder = tileReorder(mm).get

    logger.debug(gen.c.function.asStringFromExpr(lower(reorder).get))
  }

  test("tile mm") {

    // loop ordering: M -> N -> K
    val mm =
      depFun((m: Nat, n: Nat, k: Nat) =>
        fun((m`.`k`.`f32) ->: (k`.`n`.`f32) ->: (m`.`n`.`f32))
        ((a, b) =>
          map(fun(ak =>
            map(fun(bk =>
              (reduceSeq(fun((acc, y) => acc + (y.`1` * y.`2`)))(lf32(0.0f))) $
                zip(ak)(bk))) $ transpose(b) )) $ a
        )
      )

    // val M = NatIdentifier("M")
    // val N = NatIdentifier("N")
    // val K = NatIdentifier("K")

    // loop ordering: M -> K -> N
    /*
    val mmLoopMKN =
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, f32)))(a =>
          fun(ArrayType(K, ArrayType(N, f32)))(b =>
            mapSeq(fun(ak =>
              reduceSeq(
                fun((y, acc) => // y :: (f32, N.f32); acc :: N.f32
                  mapSeq(fun(t => fst(t) + snd(t))) $
                    zip(acc,
                      mapSeq(fun(bs => bs * fst(y))) $ snd(y))
                ),
                generate(fun(IndexType(N) ->: f32)(_ => l(0.0f)))
              ) $
                zip(ak, b))
            ) $ a
          )
        )
      )))
    */

    // loop ordering: M -> K -> N
    /*
    val mmLoopMKNFused =
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, f32)))(a =>
          fun(ArrayType(K, ArrayType(N, f32)))(b =>
            mapSeq(fun(ak =>
              reduceSeq(
                fun((y, acc) => // y :: (f32, N.f32); acc :: N.f32
                  mapSeq(fun(bs => (fst(bs) * fst(y)) + snd(bs))) $ zip(snd(y), acc)
                ),
                generate(fun(IndexType(N) ->: f32)(_ => l(0.0f)))
              ) $
                zip(ak, b))
            ) $ a
          )
        )
      )))
    */

    // loop ordering: K -> M -> N
    /*
    val mmOuterProduct =
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, f32)))(a =>
          fun(ArrayType(K, ArrayType(N, f32)))(b =>
            reduceSeq(
              fun((y, acc) => // y :: (M.f32, N.f32); acc :: M.N.f32
                // 3. add accumulator and computed outer product
                (mapSeq(mapSeq(fun(tuple => fst(tuple) + snd(tuple)))) o
                  // 2. zip2D accumulator + computed outer product
                  map(fun(t => zip(fst(t), snd(t))))) $ zip(acc, // here the map-acceptor translation works
                  // 1. compute one outer product
                  mapSeq(fun(am => mapSeq(fun(bn => mul(am,bn))) $ snd(y))) $ fst(y))),
              // generate zeros :: M.N.f32
              generate(fun(IndexType(M) ->: ArrayType(N, f32))(_ =>
                generate(fun(IndexType(N) ->: f32)(_ => l(0.0f) ))))
            ) $
              zip(transpose(a), b) // :: K.(M.f32, N.f32)
          ))
      )))
    */

    // loop ordering: K -> M -> N
    /*
    val mmOuterProductFusedComputation =
      depLambda[NatKind](M, depLambda[NatKind](N, depLambda[NatKind](K,
        fun(ArrayType(M, ArrayType(K, f32)))(a =>
          fun(ArrayType(K, ArrayType(N, f32)))(b =>
            reduceSeq(
              fun((y, acc) => // y :: (M.f32, N.f32); acc :: M.N.f32
                mapSeq(fun(amAccn => // amAccn :: M.(f32, N.f32)
                  mapSeq(fun(bnAcc => // bnAcc :: N.(f32, f32)
                    mul(fst(amAccn),fst(bnAcc)) + snd(bnAcc))) $
                    zip(snd(y), snd(amAccn)))) $
                  zip(fst(y), acc)),
              // generate zeros :: M.N.f32
              generate(fun(IndexType(M) ->: ArrayType(N, f32))(_ =>
                generate(fun(IndexType(N) ->: f32)(_ => l(0.0f) ))))
            ) $
              zip(transpose(a), b) // :: K.(M.f32, N.f32)
          ))
      )))
    */

    val tiled = topDown(tileND(2)(16)).apply(mm)
    tiled.get
  }

  test("reduce rows") {
    val addT = fun(x => fst(x) + snd(x))

    val test = depFun((n: Nat, m: Nat) => fun(ArrayType(n, ArrayType(m, f32)))(i =>
      reduceSeq(fun((y, acc) =>
        map(addT) $ zip(y)(acc)))(
        generate(fun(IndexType(m) ->: f32)(_ => lf32(0.0f)))) $ i))

    test.toExpr
  }

  test("dot and outer product") {
    def xsT(N : Nat) = ArrayType(N, f32)
    def ysT(N : Nat) = ArrayType(N, f32)

    val mulT = fun(x => fst(x) * snd(x))
    val add = fun(x => fun(a => x + a))

    val dot = depFun((n: Nat) => fun(xsT(n))(xs => fun(ysT(n))(ys =>
      (reduce(add)(lf32(0.0f)) o map(mulT)) $ zip(xs)(ys)
    )))

    val outer = depFun((n: Nat) => fun(xsT(n))(xs => fun(ysT(n))(ys =>
      map(fun(a => map(fun(b => add(a,b))) $ ys )) $ xs
    )))

    dot.toExpr
    outer.toExpr
  }
}
