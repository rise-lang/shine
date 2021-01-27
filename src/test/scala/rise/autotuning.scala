package rise

import arithexpr.arithmetic.{PosInf, RangeAdd}
import rise.core._
import rise.core.types._
import rise.core.primitives._
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.DSL.HighLevelConstructs.{tileShiftInwards, slideVectors}
import rise.openCL.TypedDSL._
import apps.separableConvolution2D.weightsSeqVecUnroll

class autotuning extends test_util.Tests {
  def tuningParam[A](name: String, w: NatFunctionWrapper[A]): A =
    w.f(NatIdentifier(name, isExplicit = true))
  def tuningParam[A](name: String, r: arithexpr.arithmetic.Range, w: NatFunctionWrapper[A]): A =
    w.f(NatIdentifier(name, r, isExplicit = true))

  //val vec: Nat = 4 // THIS WORKS
  val convolution: ToBeTyped[Expr] =
    //depFun((vec: Nat) => // THIS WORKS
    // FIXME: type inference goes nuts with both attempts below
    //tuningParam("vec", (vec: Nat) =>
    impl((vec: Nat) =>
    //tuningParam("tile", RangeAdd(4, 32, vec), (tile: Nat) =>
    depFun(RangeAdd(4, 32, vec), (tile: Nat) =>
    depFun(RangeAdd(1, PosInf, vec), (n: Nat) =>
    //fun(3`.`f32)(weights =>
    fun(((n+2)`.`f32) ->: (n`.`f32))(input =>
      input |> printType("a") |> tileShiftInwards(tile)(//mapWorkGroup(0)(
        /*slideVectors(vec) >> slide(3)(vec) >>
        mapLocal(0)(weightsSeqVecUnroll(weights)) >>
        asScalar*/
        typeHole("b")
      )//)
    ))))//)

  test("collect parameters") {
    println(convolution.t)
    println(autotune.collectParameters(convolution))
  }
}
