package rise

import arithexpr.arithmetic.{NamedVar, PosInf, RangeAdd, RangeMul, RangeUnknown}
import rise.core._
import rise.core.types._
import rise.core.primitives._
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.DSL.HighLevelConstructs.{slideVectors, tileShiftInwards}
import rise.openCL.DSL._
import rise.autotune.{collectConstraints, tuningParam}
import apps.separableConvolution2D.weightsSeqVecUnroll
//import shine.DPIA.Types.TypeCheck
//import util.gen


class autotuning extends test_util.Tests {
  val convolution: ToBeTyped[Expr] =
  // tileShiftInwards should constrain n >= tile
  // slideVectors and slide should constrain tile % vec = 0
    tuningParam("vec", RangeAdd(0, 32, 1), (vec: Nat) =>
      tuningParam("tile", RangeAdd(4, 32, 1), (tile: Nat) =>
        depFun(RangeAdd(1, PosInf, vec), (n: Nat) =>
          fun(3`.`f32)(weights =>
            fun(((n+2)`.`f32) ->: (n`.`f32))(input =>
              input |> tileShiftInwards(tile)(mapWorkGroup(0)(
                slideVectors(vec) >> slide(3)(vec) >>
                  mapLocal(0)(weightsSeqVecUnroll(weights)) >>
                  asScalar
              ))
            )))))

  test("collect parameters") {
    val params = autotune.collectParameters(convolution)
    assert(params.find(_.name == "vec").get.range == RangeAdd(0, 32, 1))
    assert(params.find(_.name == "tile").get.range == RangeAdd(4, 32, 1))
    assert(params.size == 2)
  }

  test("collect constraints") {
    val e: Expr = convolution
    autotune.collectConstraints(e, autotune.collectParameters(e)).foreach(println)
  }

  test("substitute parameters") {
    val e: Expr = convolution(32)
    val constraints = autotune.collectConstraints(e, autotune.collectParameters(e))

    val badParameters1 = Map(
      NatIdentifier("vec", isExplicit = true) -> (5: Nat),
      NatIdentifier("tile", isExplicit = true) -> (15: Nat)
    )
    assert(!autotune.checkConstraints(constraints, badParameters1))

    val badParameters2 = Map(
      NatIdentifier("vec", isExplicit = true) -> (4: Nat),
      NatIdentifier("tile", isExplicit = true) -> (13: Nat)
    )
    assert(!autotune.checkConstraints(constraints, badParameters2))

    /* FIXME: there is no `n >= tile` constraint collected
    val badParameters3 = Map(
      NatIdentifier("vec", isExplicit = true) -> (8: Nat),
      NatIdentifier("tile", isExplicit = true) -> (64: Nat)
    )
    assert(!autotune.checkConstraints(constraints, badParameters3))
    */

    val goodParameters = Map(
      NatIdentifier("vec", isExplicit = true) -> (4: Nat),
      NatIdentifier("tile", isExplicit = true) -> (16: Nat)
    )
    assert(autotune.checkConstraints(constraints, goodParameters))
    rise.core.substitute.natsInExpr(goodParameters, e)
  }

  test("generateJSON"){
    val json = autotune.generateJSON(autotune.collectParameters(convolution))

    // create gold
    // check against gold

    println("json: \n" + json)
  }

  test("search"){
    val result = autotune.search(convolution)
  }


}
