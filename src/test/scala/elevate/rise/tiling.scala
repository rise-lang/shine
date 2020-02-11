package elevate.rise

import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.core.{RewriteResult, Strategy}
import elevate.rise.rules._
import elevate.rise.rules.algorithmic._
import elevate.rise.rules.traversal._
import elevate.rise.strategies.normalForm._
import elevate.rise.strategies.tiling._
import elevate.rise.strategies.traversal._
import elevate.util._
import rise.core.TypedDSL._
import rise.core.TypeLevelDSL._
import rise.core._
import rise.core.types._
import rise.core.types.{ArrayType, NatKind, f32, infer}
import util.gen

import scala.language.implicitConversions

class tiling extends shine.test_util.Tests {

  implicit def rewriteResultToExpr(r: RewriteResult[Expr]): Expr = r.get

  def betaEtaEquals(a: Rise, b: Rise): Boolean = {
    val na = BENF(a).get
    val nb = BENF(b).get
    val uab: Rise = toTDSL(na) :: nb.t
    makeClosed(uab) == makeClosed(nb)
  }
  // Check that DSL makes sense

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

  // Tiling one loop

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

  // Tiling two loops

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

  // Tiling three loops

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

  // Tiling four loops

  test("tileND - tile four loops 4D") {
    val input4D: Rise = λ(i => λ(f => ****!(f) $ i))
    val gold: Rise = λ(i => λ(f => (
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

  // Codegen tests

  def inputT(dim: Int, n : List[NatIdentifier]): ArrayType = dim match {
    case 1 => ArrayType(n.head, f32)
    case d => ArrayType(n.head, inputT(d-1, n.tail))
  }

  def wrapInLambda[T <: Expr](dim: Int,
                   f: TDSL[Identifier] => TDSL[T],
                   genInputType: List[NatIdentifier] => ArrayType,
                   natIds: List[NatIdentifier] = List()): TDSL[DepLambda[NatKind]] = {
    dim match {
      case 1 => nFun(n => fun(genInputType( natIds :+ n))(f))
      case d => nFun(n => wrapInLambda(d - 1, f, genInputType, natIds :+ n))
    }
  }

  // todo: this should use mapSeqCompute and CNF instead of RNF
  // ... but mapAcceptorTranslation for split is missing
  val lower: Strategy[Rise] = LCNF `;` CNF `;` normalize.apply(lowering.mapSeq) `;` BENF

  val identity = dtFun(t => foreignFun("identity", Seq("y"), "{ return y; }", t ->: t))
  val floatId: Expr = identity(f32)

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

 // Tests related to fixing some development issues

  test("map fission issue when used with zip") {
    def xsT(N : NatIdentifier) = ArrayType(N, f32)
    def ysT(N : NatIdentifier) = ArrayType(N, f32)

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
    assert(makeClosed((LCNF `;` LCNF)(λ(i => λ(f => (J o **(f) o S) $ i))).get) == makeClosed(gold2))
    assert(makeClosed((LCNF `;` LCNF `;` LCNF)(λ(i => λ(f => (J o **(f) o S) $ i))).get) == makeClosed(gold2))

    val gold3 = (LCNF `;` RNF)(λ(i => λ(f => (J o **(f) o S) $ i))).get
    assert(makeClosed((LCNF `;` RNF `;` LCNF `;` RNF)(λ(i => λ(f => (J o **(f) o S) $ i))).get) == makeClosed(gold3))
  }
}
