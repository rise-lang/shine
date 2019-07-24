package elevate.core

import elevate.lift.strategies.traversal._
import elevate.lift.strategies.normalForm._
import elevate.core.strategies.tiling._
import elevate.core.strategies.traversal._
import elevate.core.strategies.basic._
import elevate.lift._
import elevate.lift.rules._
import idealised.util.gen
import lift.core.DSL._
import lift.core.{Apply, Expr, NatDepLambda, NatIdentifier, TypedExpr}
import lift.core.primitives._
import lift.core.types.{ArrayType, float, infer}
import org.scalatest.Ignore

import scala.language.implicitConversions


class tiling extends idealised.util.Tests {

  implicit def rewriteResultToExpr(r: RewriteResult): Expr = r.get

  test("LCNF") {
    assert(structEq(
      λ(i => λ(f => *(f) $ i)),
      λ(i => λ(f => *!(f) $ i))
    ))

    assert(structEq(
      LCNF(λ(i => λ(f => **(f) $ i))),
      λ(i => λ(f => **!(f) $ i))
    ))

    assert(structEq(
      LCNF(λ(i => λ(f => ***(f) $ i))),
      λ(i => λ(f => ***!(f) $ i))
    ))
  }

  /// TILING ONE LOOP

  test("tileND - tile one loop 1D") {
    assert(structEq(
      body(body(tileND(1)(tileSize)))(λ(i => λ(f => *(f) $ i))),
      λ(i => λ(f => (J o **(f) o S) $ i))
    ))
  }

  test("tileND - tile one loop 2D") {
    val input2D = λ(i => λ(f => **!(f) $ i))

    // outer
    assert(structEq(
      body(body(tileND(1)(tileSize)))(input2D),
      λ(i => λ(f => (J o ***(f) o S) $ i))
    ))

    // inner
    assert(structEq(
      body(body(fmap(tileND(1)(tileSize))))(input2D),
      λ(i => λ(f => *(J o **(f) o S) $ i))
    ))
  }

  test("tileND - tile one loop 3D") {
    val input3D = λ(i => λ(f => ***!(f) $ i))

    // outer
    assert(structEq(
      body(body(tileND(1)(tileSize)))(input3D),
      λ(i => λ(f => (J o ****(f) o S) $ i))
    ))

    // middle
    assert(structEq(
      body(body(fmap(tileND(1)(tileSize))))(input3D),
      λ(i => λ(f => *(J o ***(f) o S) $ i))
    ))

    // inner
    assert(structEq(
      body(body(fmap(fmap(tileND(1)(tileSize)))))(input3D),
      λ(i => λ(f => **(J o **(f) o S) $ i))
    ))
  }

  test("tileND - tile one loop 4D") {
    // dimensions: M.N.O.P (inner->outer)
    val input4D = λ(i => λ(f => ****!(f) $ i))

    // P
    assert(structEq(
      body(body(tileND(1)(tileSize)))(input4D),
      λ(i => λ(f => (J o *****(f) o S) $ i))
    ))

    // O
    assert(structEq(
      body(body(fmap(tileND(1)(tileSize))))(input4D),
      λ(i => λ(f => *(J o ****(f) o S) $ i))
    ))

    // N
    assert(structEq(
      body(body(fmap(fmap(tileND(1)(tileSize)))))(input4D),
      λ(i => λ(f => **(J o ***(f) o S) $ i))
    ))

    // M
    assert(structEq(
      body(body(fmap(fmap(fmap(tileND(1)(tileSize))))))(input4D),
      λ(i => λ(f => ***(J o **(f) o S) $ i))
    ))
  }

  /// TILING TWO LOOPS

  test("tileND - tile two loops 2D") {
    assert(structEq(
      body(body(tileND(2)(tileSize)))(λ(i => λ(f => **!(f) $ i))),
      λ(i => λ(f => (J o **(J) o *(T) o ****(f) o *(T) o **(S) o S) $ i))
    ))
  }

  test("tileND - tile two loops 3D") {
    val input3D = λ(i => λ(f => ***!(f) $ i))

    // outer two
    assert(structEq(
      body(body(tileND(2)(tileSize)))(input3D),
      LCNF(λ(i => λ(f => (J o **(J) o *(T) o *****(f) o *(T) o **(S) o S) $ i)))
    ))

    // inner two
    assert(structEq(
      body(body(fmap(tileND(2)(tileSize))))(input3D),
      LCNF(λ(i => λ(f => *(J o **(J) o *(T) o ****(f) o *(T) o **(S) o S) $ i)))
    ))
  }

  test("tileND - tile two loops 4D") {
    val input4D = λ(i => λ(f => ****!(f) $ i))

    // outer two
    assert(structEq(
      body(body(tileND(2)(tileSize)))(input4D),
      λ(i => λ(f => (J o **(J) o *(T) o ******(f) o *(T) o **(S) o S) $ i))
    ))

    // middle two
    assert(structEq(
      body(body(fmap(tileND(2)(tileSize))))(input4D),
      λ(i => λ(f => *(J o **(J) o *(T) o *****(f) o *(T) o **(S) o S) $ i))
    ))

    // inner two
    assert(structEq(
      body(body(fmap(fmap(tileND(2)(tileSize)))))(input4D),
      λ(i => λ(f => **(J o **(J) o *(T) o ****(f) o *(T) o **(S) o S) $ i))
    ))
  }

  /// TILING THREE LOOPS

  test("tileND - tile three loops 3D") {
    assert(structEq(
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
    assert(structEq(
      body(body(tileND(3)(tileSize)))(input4D),
      λ(i => λ(f => (
      J o **(J) o ****(J) o
      ***(T) o *(T) o **(T) o
      *(******(f)) o
      **(T) o *(T) o ***(T) o
      ****(S) o **(S) o S) $ i))
    ))

    // inner three
    assert(structEq(
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

    assert(structEq(
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
                   f: TypedExpr => Expr,
                   genInputType: List[NatIdentifier] => ArrayType,
                   natIds: List[NatIdentifier] = List()): NatDepLambda = {
    dim match {
      case 1 => nFun(n => fun(genInputType( natIds :+ n))(f))
      case d => nFun(n => wrapInLambda(d - 1, f, genInputType, natIds :+ n))
    }
  }

  // todo: this should use mapSeqCompute and CNF instead of RNF
  // ... but mapAcceptorTranslation for split is missing
  val lower: Strategy = LCNF `;` RNF `;` normalize(specialize.mapSeq) `;` BENF

  val identity = tFun(t => foreignFun("identity", Seq("y"), "{ return y; }", t ->: t))
  val floatId: Expr = identity(float)

  test("codegen 1D tiles") {
    val highLevel = wrapInLambda(1, i => *(floatId) $ i, inputT(1, _))
    val tiled = one(body(tileND(1)(tileSize)))(highLevel).get

    println(gen.CProgram(lower(highLevel)))
    println(gen.CProgram(lower(tiled)))
  }

  test("codegen 2D tiles") {
    val highLevel = wrapInLambda(2, i => **!(floatId) $ i, inputT(2, _))
    val tiled = one(one(body(tileND(2)(tileSize))))(highLevel).get

    println(gen.CProgram(lower(highLevel)))
    println(gen.CProgram(lower(tiled)))
  }

  test("codegen 3D tiles") {
    val highLevel = wrapInLambda(3, i => ***!(floatId) $ i, inputT(3, _))
    val tiled = one(one(one(body(tileNDList(3)(List(4,8,16))))))(highLevel).get

    println(gen.CProgram(lower(highLevel)))
    println(gen.CProgram(lower(tiled)))
  }

  test("codegen two innermost of three loops") {
    val highLevel = wrapInLambda(3, i => ***!(floatId) $ i, inputT(3, _))
    val tiled = one(one(one(body(fmap(tileND(2)(tileSize))))))(highLevel).get

    println(gen.CProgram(lower(highLevel)))
    println(gen.CProgram(lower(tiled)))
  }

  /// LOOP INTERCHANGE

   test("simple loop interchange") {
     assert(structEq(
       body(body(loopInterchange))(λ(i => λ(f => **!(f) $ i))),
       λ(i => λ(f => (T o **(f) o T) $ i))
     ))
   }

  test("interchange innermost two loops in loop nest of depth 3") {
    assert(structEq(
      body(body(loopInterchangeAtLevel(1)))(λ(i => λ(f => ***!(f) $ i))),
      λ(i => λ(f => (*(T) o ***(f) o *(T)) $ i))
    ))

    assert(structEq(
      body(body(fmap(loopInterchange) `;` LCNF `;` RNF))(λ(i => λ(f => ***!(f) $ i))),
      λ(i => λ(f => (*(T) o ***(f) o *(T)) $ i))
    ))
   }
}
