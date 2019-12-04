package elevate.core

import elevate.rise.rules.traversal._
import elevate.rise.strategies.traversal._
import elevate.rise.strategies.normalForm._
import elevate.rise.strategies.tiling._
import elevate.core.strategies.traversal._
import elevate.core.strategies.basic._
import elevate.rise._
import elevate.util._
import elevate.rise.rules._
import elevate.rise.meta.fission._
import elevate.rise.rules.algorithmic._
import util.gen
import lift.core.DSL._
import lift.core._
import lift.core.types.{ArrayType, NatKind, float, infer}

import scala.language.implicitConversions

class tiling extends test_util.Tests {

  implicit def rewriteResultToExpr(r: RewriteResult[Expr]): Expr = r.get

  test("LCNF") {
    assert(betaEtaEquals(
      λ(i => λ(f => *(f) $ i)),
      λ(i => λ(f => *!(f) $ i))
    ))

    assert(betaEtaEquals(
      LCNF(λ(i => λ(f => **(f) $ i))),
      λ(i => λ(f => **!(f) $ i))
    ))

    assert(betaEtaEquals(
      LCNF(λ(i => λ(f => ***(f) $ i))),
      λ(i => λ(f => ***!(f) $ i))
    ))
  }

  /// TILING ONE LOOP

  test("tileND - tile one loop 1D") {
    println(body(body(tileND(1)(tileSize)) `;` BENF)(λ(i => λ(f => *(f) $ i))).toString)
    println(λ(i => λ(f => (J o **(f) o S) $ i)).toString)
    assert(betaEtaEquals(
      body(body(tileND(1)(tileSize)))(λ(i => λ(f => *(f) $ i))),
      λ(i => λ(f => (J o **(f) o S) $ i))
    ))
  }

  test("tileND - tile one loop 2D") {
    val input2D = λ(i => λ(f => **!(f) $ i))

    // outer
    assert(betaEtaEquals(
      body(body(tileND(1)(tileSize)))(input2D),
      λ(i => λ(f => (J o ***(f) o S) $ i))
    ))

    // inner
    assert(betaEtaEquals(
      body(body(fmap(tileND(1)(tileSize))))(input2D),
      λ(i => λ(f => *(J o **(f) o S) $ i))
    ))
  }

  test("tileND - tile one loop 3D") {
    val input3D = λ(i => λ(f => ***!(f) $ i))

    // outer
    assert(betaEtaEquals(
      body(body(tileND(1)(tileSize)))(input3D),
      λ(i => λ(f => (J o ****(f) o S) $ i))
    ))

    // middle
    assert(betaEtaEquals(
      body(body(fmap(tileND(1)(tileSize))))(input3D),
      λ(i => λ(f => *(J o ***(f) o S) $ i))
    ))

    // inner
    assert(betaEtaEquals(
      body(body(fmap(fmap(tileND(1)(tileSize)))))(input3D),
      λ(i => λ(f => **(J o **(f) o S) $ i))
    ))
  }

  test("tileND - tile one loop 4D") {
    // dimensions: M.N.O.P (inner->outer)
    val input4D = λ(i => λ(f => ****!(f) $ i))

    // P
    assert(betaEtaEquals(
      body(body(tileND(1)(tileSize)))(input4D),
      λ(i => λ(f => (J o *****(f) o S) $ i))
    ))

    // O
    assert(betaEtaEquals(
      body(body(fmap(tileND(1)(tileSize))))(input4D),
      λ(i => λ(f => *(J o ****(f) o S) $ i))
    ))

    // N
    assert(betaEtaEquals(
      body(body(fmap(fmap(tileND(1)(tileSize)))))(input4D),
      λ(i => λ(f => **(J o ***(f) o S) $ i))
    ))

    // M
    assert(betaEtaEquals(
      body(body(fmap(fmap(fmap(tileND(1)(tileSize))))))(input4D),
      λ(i => λ(f => ***(J o **(f) o S) $ i))
    ))
  }

  /// TILING TWO LOOPS

  test("tileND - tile two loops 2D") {
    assert(betaEtaEquals(
      body(body(tileND(2)(tileSize)))(λ(i => λ(f => **!(f) $ i))),
      λ(i => λ(f => (J o **(J) o *(T) o ****(f) o *(T) o **(S) o S) $ i))
    ))
  }

  test("tileND - tile two loops 3D") {
    val input3D = λ(i => λ(f => ***!(f) $ i))

    // outer two
    assert(betaEtaEquals(
      body(body(tileND(2)(tileSize)))(input3D),
      LCNF(λ(i => λ(f => (J o **(J) o *(T) o *****(f) o *(T) o **(S) o S) $ i)))
    ))

    // inner two
    assert(betaEtaEquals(
      body(body(fmap(tileND(2)(tileSize))))(input3D),
      LCNF(λ(i => λ(f => *(J o **(J) o *(T) o ****(f) o *(T) o **(S) o S) $ i)))
    ))
  }

  test("tileND - tile two loops 4D") {
    val input4D = λ(i => λ(f => ****!(f) $ i))

    // outer two
    assert(betaEtaEquals(
      body(body(tileND(2)(tileSize)))(input4D),
      λ(i => λ(f => (J o **(J) o *(T) o ******(f) o *(T) o **(S) o S) $ i))
    ))

    // middle two
    assert(betaEtaEquals(
      body(body(fmap(tileND(2)(tileSize))))(input4D),
      λ(i => λ(f => *(J o **(J) o *(T) o *****(f) o *(T) o **(S) o S) $ i))
    ))

    // inner two
    assert(betaEtaEquals(
      body(body(fmap(fmap(tileND(2)(tileSize)))))(input4D),
      λ(i => λ(f => **(J o **(J) o *(T) o ****(f) o *(T) o **(S) o S) $ i))
    ))
  }

  /// TILING THREE LOOPS

  test("tileND - tile three loops 3D") {
    assert(betaEtaEquals(
      body(body(tileND(3)(tileSize)))(λ(i => λ(f => ***!(f) $ i))),
      λ(i => λ(f => (
      J o **(J) o ****(J) o
      ***(T) o *(T) o **(T) o
      ******(f) o
      **(T) o *(T) o ***(T) o
      ****(S) o **(S) o S) $ i))
    ))
  }

  test("tileND - tile three loops 4D") {
    val input4D = λ(i => λ(f => ****!(f) $ i))

    // outer three
    assert(betaEtaEquals(
      body(body(tileND(3)(tileSize)))(input4D),
      λ(i => λ(f => (
      J o **(J) o ****(J) o
      ***(T) o *(T) o **(T) o
      *(******(f)) o
      **(T) o *(T) o ***(T) o
      ****(S) o **(S) o S) $ i))
    ))

    // inner three
    assert(betaEtaEquals(
      body(body(fmap(tileND(3)(tileSize))))(input4D),
      λ(i => λ(f => *(
      J o **(J) o ****(J) o
      ***(T) o *(T) o **(T) o
      ******(f) o
      **(T) o *(T) o ***(T) o
      ****(S) o **(S) o S) $ i))
    ))
  }

  /// TILING FOUR LOOPS

  test("tileND - tile four loops 4D") {
    val input4D = λ(i => λ(f => ****!(f) $ i))
    val gold = λ(i => λ(f => (
      J o **(J) o ****(J) o ******(J) o
        *****(T) o ***(T) o ****(T) o *(T) o **(T) o ***(T) o
        ****(****(f)) o
        ***(T) o **(T) o *(T) o ****(T) o ***(T) o *****(T) o
        ******(S) o ****(S) o **(S) o S) $ i
      ))

    assert(betaEtaEquals(
      body(body(tileND(4)(tileSize)))(input4D),
      gold
    ))
  }

  /// CODEGEN TESTS

  def inputT(dim: Int, n : List[NatIdentifier]): ArrayType = dim match {
    case 1 => ArrayType(n.head, float)
    case d => ArrayType(n.head, inputT(d-1, n.tail))
  }

  def wrapInLambda(dim: Int,
                   f: Expr => Expr,
                   genInputType: List[NatIdentifier] => ArrayType,
                   natIds: List[NatIdentifier] = List()): DepLambda[NatKind] = {
    dim match {
      case 1 => nFun(n => fun(genInputType( natIds :+ n))(f))
      case d => nFun(n => wrapInLambda(d - 1, f, genInputType, natIds :+ n))
    }
  }

  // todo: this should use mapSeqCompute and CNF instead of RNF
  // ... but mapAcceptorTranslation for split is missing
  val lower: Strategy[Rise] = LCNF `;` CNF `;` normalize.apply(lowering.mapSeq) `;` BENF

  val identity = dtFun(t => foreignFun("identity", Seq("y"), "{ return y; }", t ->: t))
  val floatId: Expr = identity(float)

  test("codegen 1D tiles") {
    val highLevel = infer(wrapInLambda(1, i => *(floatId) $ i, inputT(1, _)))
    val tiled = infer(one(body(tileND(1)(tileSize))).apply(highLevel).get)

    println(gen.CProgram(lower(highLevel)))
    println(gen.CProgram(lower(tiled)))
  }

  //TODO make this work without implicit array assignments
  ignore("codegen 2D tiles") {
    val highLevel = wrapInLambda(2, i => **!(floatId) $ i, inputT(2, _))
    val tiled = one(one(body(tileND(2)(tileSize)))).apply(highLevel).get

    println(gen.CProgram(lower(highLevel)))
    println(gen.CProgram(lower(tiled)))
  }

  //TODO make this work without implicit array assignments
  ignore("codegen 3D tiles") {
    val highLevel = wrapInLambda(3, i => ***!(floatId) $ i, inputT(3, _))
    val tiled = one(one(one(body(tileNDList(List(4,8,16)))))).apply(highLevel).get

    println(gen.CProgram(lower(highLevel)))
    println(gen.CProgram(lower(tiled)))
  }

  //TODO make this work without implicit array assignments
  ignore("codegen two innermost of three loops") {
    val highLevel = wrapInLambda(3, i => ***!(floatId) $ i, inputT(3, _))
    val tiled = one(one(one(body(fmap(tileND(2)(tileSize)))))).apply(highLevel).get


    println(gen.CProgram(lower(highLevel)))
    println(gen.CProgram(lower(tiled)))
  }

 /// REAL APPLICATIONS

  // todo WIP
  test("tile gemm") {
    val backward =
      nFun((m, n, k) =>
        fun((m`.`k`.`float) ->: (k`.`n`.`float) ->: (m`.`n`.`float) ->: float ->: float ->: (m`.`n`.`float))
        ((a, b, c, alpha, beta) =>
          map(fun(ac =>
            map(fun(bc =>
              (fun(x => (x * alpha) + beta * bc._2) o
                reduceSeq(fun(acc => fun(y => acc + (y._1 * y._2))), l(0.0f))) $
            zip(ac._1, bc._1))) $
          zip(transpose(b),ac._2))) $
        zip(a, c)
        )
      )

    val tiling = applyNTimes(3)(body)(applyNTimes(5)(body)(tileND(2)(4)))

    val normalized = FNF(tiling).get
    val rewritten = normalized(backward)

    val debug =
      body(body(body(body(body(body(body(body(function(argumentOf(map,body(function(splitJoin(4))))))))))))) `;`
      body(body(body(body(body(body(body(body(function(splitJoin(4)))))))))) `;`
        body(body(body(body(body(body(body(body(RNF)))))))) `;`
        body(body(body(body(body(body(body(body(LCNF)))))))) `;`
        body(body(body(body(body(body(body(body(argument(argument(function(argumentOf(map,body(idAfter))))))))))))) `;`
        body(body(body(body(body(body(body(body(argument(argument(function(argumentOf(map,body(createTransposePair))))))))))))) `;`
        body(body(body(body(body(body(body(body(argument(argument(function(argumentOf(map,body(LCNF)))))))))))))
        //body(body(body(inTyped(body(body(body(body(body(argument(argument(function(argumentOf(map,body(argument(mapMapFBeforeTranspose))))))))))))))) `;`
        //body(body(body(inTyped(body(body(body(body(body(argument(argument(RNF)))))))))))

    val result = debug(backward)
    val types = infer(result)
  }

  test("map fission issue when used with zip") {
    def xsT(N : NatIdentifier) = ArrayType(N, float)
    def ysT(N : NatIdentifier) = ArrayType(N, float)

    val mulT = fun(x => fst(x) * snd(x))

    val simple = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
        zip(xs)(ys) |> map(mulT)
    )))

    // we can't fission the map
    assert(betaEtaEquals(infer((RNF)(simple)), infer(simple)))
    // and tiling it doesn't break it either
    infer((oncetd(splitJoin(4)) `;` RNF `;` LCNF)(simple))
    infer((oncetd(tileND(1)(4)) `;` RNF `;` LCNF)(simple))
  }

  test("normalform actually normalizes") {
    val gold = λ(i => λ(f => (J o **(f) o S) $ i))
    assert(betaEtaEquals((RNF `;` BENF) (λ(i => λ(f => (J o **(f) o S) $ i))), gold))
    assert(betaEtaEquals((RNF `;` RNF `;` BENF) (λ(i => λ(f => (J o **(f) o S) $ i))), gold))
    assert(betaEtaEquals((RNF `;` RNF `;` RNF `;` BENF) (λ(i => λ(f => (J o **(f) o S) $ i))), gold))

    val gold2 = LCNF(λ(i => λ(f => (J o **(f) o S) $ i))).get
    assert((LCNF `;` LCNF)(λ(i => λ(f => (J o **(f) o S) $ i))).get == gold2)
    assert((LCNF `;` LCNF `;` LCNF)(λ(i => λ(f => (J o **(f) o S) $ i))).get == gold2)

    val gold3 = (LCNF `;` RNF)(λ(i => λ(f => (J o **(f) o S) $ i))).get
    assert((LCNF `;` RNF `;` LCNF `;` RNF)(λ(i => λ(f => (J o **(f) o S) $ i))).get == gold3)
  }
}
