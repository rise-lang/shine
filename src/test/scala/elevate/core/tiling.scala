package elevate.core

import java.io.{File, PrintWriter}

import elevate.core.strategies.basic
import elevate.lift.strategies.traversal._
import elevate.lift.strategies.normalForm._
import elevate.core.strategies.tiling._
import elevate.core.strategies.traversal._
import elevate.core.strategies.basic._
import elevate.lift._
import elevate.util._
import elevate.lift.rules._
import elevate.lift.rules.algorithmic._
import idealised.util.gen
import lift.core.DSL._
import lift.core.{Apply, DepLambda, Expr, Identifier, Lambda, NatIdentifier, TypedExpr}
import lift.core.primitives._
import lift.core.types.{ArrayType, NatKind, float, infer}
import org.scalatest.Ignore

import scala.language.implicitConversions

class tiling extends idealised.util.Tests {

  implicit def rewriteResultToExpr(r: RewriteResult): Expr = r.get

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
    println(body(body(tileND(1)(tileSize)))(λ(i => λ(f => *(f) $ i))))
    println(λ(i => λ(f => (J o **(f) o S) $ i)))
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
                   f: TypedExpr => Expr,
                   genInputType: List[NatIdentifier] => ArrayType,
                   natIds: List[NatIdentifier] = List()): DepLambda[NatKind] = {
    dim match {
      case 1 => nFun(n => fun(genInputType( natIds :+ n))(f))
      case d => nFun(n => wrapInLambda(d - 1, f, genInputType, natIds :+ n))
    }
  }

  // todo: this should use mapSeqCompute and CNF instead of RNF
  // ... but mapAcceptorTranslation for split is missing
  val lower: Strategy = LCNF `;` RNF `;` normalize(specialize.mapSeq) `;` BENF

  val identity = dtFun(t => foreignFun("identity", Seq("y"), "{ return y; }", t ->: t))
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
    val tiled = one(one(one(body(tileNDList(List(4,8,16))))))(highLevel).get

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
                reduce(fun((y, acc) => acc + (y._1 * y._2)), l(0.0f))) $
            zip(ac._1, bc._1))) $
          zip(transpose(b),ac._2))) $
        zip(a, c)
        )
      )

    val tiled = (LCNF `;` CNF `;` oncetd(tileND(2)(4)) `;` BENF `;` RNF)(backward)
    //infer(tiled)
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
    assert(betaEtaEquals((RNF `;` BENF)(λ(i => λ(f => (J o **(f) o S) $ i))), gold))
    assert(betaEtaEquals((RNF `;` RNF `;` BENF)(λ(i => λ(f => (J o **(f) o S) $ i))), gold))
    assert(betaEtaEquals((RNF `;` RNF `;` RNF `;` BENF)(λ(i => λ(f => (J o **(f) o S) $ i))), gold))
  }
}
