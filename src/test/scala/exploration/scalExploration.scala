package exploration

import rise.core.DSL._
import rise.core.primitives._
import Type._
import rise.core.types._
import rise.core.types.DataType._


object scalExploration {

  val scal = depFun((n: Nat) => fun(n`.`f32)(input => fun(f32)(alpha =>
    input |> map(fun(x => alpha * x)))
  ))

  // different lowering strategies
  // strategies introducing tuning parameters
  // map -> mapSeq (all to mapSeq)
  // map, (map, ...) -> MapGlb, (mapSeq, mapSeq, ...) (first mapGlb, rest mapSeq)
  // map, map, (map, ...) -> MapLcl, MapWrg, mapSeq (first mapLcl, second mapWrg, rest mapSeq)

  def main(args: Array[String]): Unit = {
    // start exploration here
    riseExploration(scal, "exploration/configuration/scal.json")
  }
}
