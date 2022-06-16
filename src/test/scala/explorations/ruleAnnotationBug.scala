package explorations

import apps.separableConvolution2D.mulT
import elevate.core.strategies.traversal.{topDown}
import elevate.core.{Strategy}
import elevate.macros.RuleMacro.rule
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._
import rise.elevate.rules.algorithmic.{splitJoin}
import rise.elevate.rules.traversal.default.RiseTraversable
import rise.elevate.{Rise, tunable}

object ruleAnnotationBug {

  // sub expressions
  val mult = impl { dt: DataType => fun(x => x._1 * x._2) :: ((dt x dt) ->: dt) }
  val add = fun(x => fun(y => x + y))

  // main expressions
  val mvHighLevel = depFun((n: Nat, m: Nat) => fun(
    (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
  )((mat, xs) =>
    mat |> map(fun(row =>
      zip(row)(xs) |> map(mulT) |> reduce(add)(lf32(0.0f))
    ))
  ))


  @rule def topDownSplitJoinAsRule: Strategy[Rise] = topDown(tunable(splitJoin))

  def topDownSplitJoin: Strategy[Rise] = topDown(tunable(splitJoin))

  def main(args: Array[String]): Unit = {

    println("highLevel: " + mvHighLevel)

    // rewrite with strategy
    val sj0 = topDownSplitJoin.apply(mvHighLevel).get
    println("sj0: \n" + sj0)
    val sj1 = topDownSplitJoin.apply(sj0).get
    println("sj1: \n" + sj1)

    // rewrite with rule annotated strategy
    val sjRule0 = topDownSplitJoinAsRule.apply(mvHighLevel).get
    println("sjRule0: \n" + sjRule0)
    val sjRule1 = topDownSplitJoinAsRule.apply(sjRule0).get
    println("sjRule1: \n" + sjRule1)

  }

}
