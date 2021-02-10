package rise

import arithexpr.arithmetic.{NamedVar, PosInf, RangeAdd, RangeMul, RangeUnknown}
import rise.core._
import rise.core.types._
import rise.core.primitives._
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.DSL.HighLevelConstructs.{slideVectors, tileShiftInwards}
import rise.openCL.TypedDSL._
import rise.autotune.tuningParam
import apps.separableConvolution2D.weightsSeqVecUnroll

class autotuning extends test_util.Tests {
  val convolution: ToBeTyped[Expr] =
      tuningParam("vec", RangeMul(1, 32, 2), (vec:Nat) =>
        tuningParam("tile", RangeAdd(4, 32, vec), (tile: Nat) =>
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

    assert(params.find(_.name == "vec").get.range == RangeUnknown)
    assert(params.find(_.name == "tile").get.range == RangeAdd(4, 32, NamedVar("vec")))
    assert(params.size == 2)
  }

  test("generateJSON"){
    val json = autotune.generateJSON(autotune.collectParameters(convolution))

    // create gold
    // check against gold

    println("json: \n" + json)
  }

  test("generate constraints"){
    val constraints = autotune.generateConstraints(autotune.collectParameters(convolution))

    println("constraints: \n" + constraints)
  }
}
