package rise.elevate

import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.core.{RewriteResult, Strategy}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.primitives._
import rise.core._
import rise.core.types.{ArrayType, NatKind, f32, _}
import rise.elevate.rules._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.traversal._
import rise.elevate.util._
import _root_.util.gen

import scala.collection.immutable
import scala.language.implicitConversions

class tiling extends test_util.Tests {

  val BENF = rise.elevate.strategies.normalForm.BENF()(default.RiseTraversable)
  val DFNF = rise.elevate.strategies.normalForm.DFNF()(default.RiseTraversable)
  val CNF = rise.elevate.strategies.normalForm.CNF()(default.RiseTraversable)
  val RNF = rise.elevate.strategies.normalForm.RNF()(default.RiseTraversable)

  def tileND = rise.elevate.strategies.tiling.tileND(default.RiseTraversable)
  def tileNDList = rise.elevate.strategies.tiling.tileNDList(default.RiseTraversable)

  implicit def rewriteResultToExpr(r: RewriteResult[Expr]): Expr = r.get

  def betaEtaEquals(a: Rise, b: Rise): Boolean = {
    val na = BENF(a).get
    val nb = BENF(b).get
    val uab: Rise = toBeTyped(na) !: nb
    makeClosed(uab) =~~= makeClosed(nb)
  }
  // Check that DSL makes sense

  test("DFNF") {
    assert(betaEtaEquals(
      λ(i => λ(f => *(f) $ i)),
      λ(i => λ(f => *!(f) $ i))
    ))

    assert(betaEtaEquals(
      DFNF(λ(i => λ(f => **(f) $ i))),
      λ(i => λ(f => **!(f) $ i))
    ))

    assert(betaEtaEquals(
      DFNF(λ(i => λ(f => ***(f) $ i))),
      λ(i => λ(f => ***!(f) $ i))
    ))
  }

  // Tiling one loop

  test("tileND - tile one loop 1D") {
    logger.debug(body(body(tileND(1)(tileSize)) `;` BENF)(λ(i => λ(f => *(f) $ i))).toString)
    logger.debug(λ(i => λ(f => (J o **(f) o S) $ i)).toString)
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
      DFNF(λ(i => λ(f => (J o **(J) o *(T) o *****(f) o *(T) o **(S) o S) $ i)))
    ))

    // inner two
    assert(betaEtaEquals(
      body(body(fmap(tileND(2)(tileSize))))(input3D),
      DFNF(λ(i => λ(f => *(J o **(J) o *(T) o ****(f) o *(T) o **(S) o S) $ i)))
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

  def inputT(dim: Int, n : List[Nat]): ArrayType = dim match {
    case 1 => ArrayType(n.head, f32)
    case d => ArrayType(n.head, inputT(d-1, n.tail))
  }

  def wrapInLambda[T <: Expr](dim: Int,
                              f: ToBeTyped[Identifier] => ToBeTyped[T],
                              genInputType: List[Nat] => ArrayType,
                              natIds: List[Nat] = List()): ToBeTyped[DepLambda[NatKind]] = {
    dim match {
      case 1 => depFun((n: Nat) => fun(genInputType( natIds :+ n))(f))
      case d => depFun((n: Nat) => wrapInLambda(d - 1, f, genInputType, natIds :+ n))
    }
  }

  // todo: this should use mapSeqCompute and CNF instead of RNF
  // ... but mapAcceptorTranslation for split is missing
  val lower: Strategy[Rise] = DFNF `;` CNF `;` normalize.apply(lowering.mapSeq) `;` BENF

  val identity = depFun((t: DataType) => foreignFun("identity", immutable.Seq("y"), "{ return y; }", t ->: t))
  val floatId: Expr = identity(f32)

  test("codegen 1D tiles") {
    val highLevel = wrapInLambda(1, i => *(floatId) $ i, inputT(1, _)).toExpr
    val tiled = one(body(tileND(1)(tileSize))).apply(highLevel).get

    logger.debug(gen.c.function.asStringFromExpr(lower(highLevel)))
    logger.debug(gen.c.function.asStringFromExpr(lower(tiled)))
  }

  //TODO make this work without implicit array assignments
  ignore("codegen 2D tiles") {
    val highLevel = wrapInLambda(2, i => **!(floatId) $ i, inputT(2, _))
    val tiled = one(one(body(tileND(2)(tileSize)))).apply(highLevel).get

    logger.debug(gen.c.function.asStringFromExpr(lower(highLevel)))
    logger.debug(gen.c.function.asStringFromExpr(lower(tiled)))
  }

  //TODO make this work without implicit array assignments
  ignore("codegen 3D tiles") {
    val highLevel = wrapInLambda(3, i => ***!(floatId) $ i, inputT(3, _))
    val tiled = one(one(one(body(tileNDList(List(4,8,16)))))).apply(highLevel).get

    logger.debug(gen.c.function.asStringFromExpr(lower(highLevel)))
    logger.debug(gen.c.function.asStringFromExpr(lower(tiled)))
  }

  //TODO make this work without implicit array assignments
  ignore("codegen two innermost of three loops") {
    val highLevel = wrapInLambda(3, i => ***!(floatId) $ i, inputT(3, _))
    val tiled = one(one(one(body(fmap(tileND(2)(tileSize)))))).apply(highLevel).get


    logger.debug(gen.c.function.asStringFromExpr(lower(highLevel)))
    logger.debug(gen.c.function.asStringFromExpr(lower(tiled)))
  }

 // Tests related to fixing some development issues

  test("map fission issue when used with zip") {
    def xsT(N : Nat) = ArrayType(N, f32)
    def ysT(N : Nat) = ArrayType(N, f32)

    val mulT = fun(x => fst(x) * snd(x))

    val simple = depFun((n: Nat) => fun(xsT(n))(xs => fun(ysT(n))(ys =>
        zip(xs)(ys) |> map(mulT)
    ))).toExpr

    // we can't fission the map
    assert(betaEtaEquals(RNF(simple), simple))
    // and tiling it doesn't break it either
    (topDown(splitJoin(4)) `;` RNF `;` DFNF)(simple)
    (topDown(tileND(1)(4)) `;` RNF `;` DFNF)(simple)
  }

  test("normalform actually normalizes") {
    val gold = λ(i => λ(f => (J o **(f) o S) $ i))
    assert(betaEtaEquals((RNF `;` BENF) (λ(i => λ(f => (J o **(f) o S) $ i))), gold))
    assert(betaEtaEquals((RNF `;` RNF `;` BENF) (λ(i => λ(f => (J o **(f) o S) $ i))), gold))
    assert(betaEtaEquals((RNF `;` RNF `;` RNF `;` BENF) (λ(i => λ(f => (J o **(f) o S) $ i))), gold))

    val gold2 = DFNF(λ(i => λ(f => (J o **(f) o S) $ i))).get
    assert(makeClosed((DFNF `;` DFNF)(λ(i => λ(f => (J o **(f) o S) $ i))).get) =~= makeClosed(gold2))
    assert(makeClosed((DFNF `;` DFNF `;` DFNF)(λ(i => λ(f => (J o **(f) o S) $ i))).get) =~= makeClosed(gold2))

    val gold3 = (DFNF `;` RNF)(λ(i => λ(f => (J o **(f) o S) $ i))).get
    assert(makeClosed((DFNF `;` RNF `;` DFNF `;` RNF)(λ(i => λ(f => (J o **(f) o S) $ i))).get) =~= makeClosed(gold3))
  }
}
