package exploration

import rise.core.DSL._
import rise.core.primitives._
import Type._
import arithexpr.arithmetic.{RangeAdd, RangeMul}
import elevate.core.{Strategy, Success}
import elevate.core.strategies.traversal.topDown
import elevate.macros.RuleMacro.rule
import rise.autotune.tuningParam
import rise.core.App
import rise.core.types._
import rise.core.types.DataType._
import rise.elevate.Rise
import rise.elevate.rules.traversal.default.RiseTraversable

object scalExploration {

  val scal = depFun((n: Nat) => fun(n`.`f32)(input => fun(f32)(alpha =>
    input |> map(fun(x => alpha * x)))
  ))

  // split join introducing tuning parameters
  def  splitJoin3(n: Nat): Strategy[Rise] = `*f -> S >> **f >> J`(n: Nat)
  @rule def `*f -> S >> **f >> J`(n: Nat): Strategy[Rise] = {
    case e @ App(map(), f) => {
      Success(
        // maybe we need a fresh name for n here
        tuningParam("n", RangeAdd(1, 1024, 1), (n: Nat) =>
          split(n) >> map(map(f)) >> join) !: e.t
      )
    }
  }

  val sjStrategy = topDown(splitJoin3(32))(RiseTraversable)
  val rewritten = sjStrategy.apply(scal)
  val rewritten2 = sjStrategy.apply(rewritten.get)

  println("scal: " + scal )
  println("rewritten: " + rewritten)
  println("rewritten2: " + rewritten2)

  // different lowering strategies
  // strategies introducing tuning parameters
  // map -> mapSeq (all to mapSeq)
  // map, (map, ...) -> MapGlb, (mapSeq, mapSeq, ...) (first mapGlb, rest mapSeq)
  // map, map, (map, ...) -> MapLcl, MapWrg, mapSeq (first mapLcl, second mapWrg, rest mapSeq)

  def main(args: Array[String]): Unit = {
    // start exploration here

  }
}
