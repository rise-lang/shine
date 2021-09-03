package exploration

import rise.core.DSL._
import rise.core.primitives._
import Type._
import elevate.core.strategies.traversal.bottomUp
import rise.core.types._
import rise.core.types.DataType._
import rise.elevate.rules.algorithmic.splitJoin
import rise.elevate.rules.traversal.default.RiseTraversable
import rise.elevate.tunable


object scalExploration {

  val scal = depFun((n: Nat) => fun(n`.`f32)(input => fun(f32)(alpha =>
    input |> map(fun(x => alpha * x)))
  ))

  val sjtp = bottomUp(tunable(splitJoin))(RiseTraversable)
  val scal2 = sjtp.apply(scal).get
  val scal3 = sjtp.apply(scal2).get
  val scal4 = sjtp.apply(scal3).get

  // try differnt lowering options
  // 1D
  val lowered1 = exploration.strategies.scalStrategies.lowerGs.apply(scal)
  val lowered2 = exploration.strategies.scalStrategies.lowerWrgLcl.apply(scal2)

  // 2D
  val lowered3 = exploration.strategies.scalStrategies.lowerGsGs.apply(scal2)
  val lowered4 = exploration.strategies.scalStrategies.lowerWrgWrgLclLcl.apply(scal4)

  println("lowered1: " + lowered1)
  println("lowered2: " + lowered2)
  println("lowered3: " + lowered3)
  println("lowered4: " + lowered4)

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
