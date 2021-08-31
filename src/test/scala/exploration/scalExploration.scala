package exploration

import rise.core.DSL._
import rise.core.primitives._
import Type._
import arithexpr.arithmetic.{RangeAdd, RangeMul, RangeUnknown}
import elevate.core.{Strategy, Success}
import elevate.core.strategies.traversal.{somebu, sometd, topDown}
import elevate.macros.RuleMacro.rule
import rise.autotune
import rise.autotune.tuningParam
import rise.core.App
import rise.core.types._
import rise.core.types.DataType._
import rise.elevate.Rise
import rise.elevate.rules.algorithmic.splitJoin
import rise.elevate.rules.traversal.default.RiseTraversable

object scalExploration {

  val scal = depFun((n: Nat) => fun(n`.`f32)(input => fun(f32)(alpha =>
    input |> map(fun(x => alpha * x)))
  ))

  // how to inject range information?
  // do we need to inject range information?
  def splitJoinTP: Strategy[Rise]  = {
    case e => {
      val freshName = rise.core.freshName.apply("tp")
      println("freshName: " + freshName)

      tuningParam(freshName, RangeUnknown, (n: Nat) =>
        splitJoin(n).apply(e)
      )
    }
  }

  // how to 2D?
  def tunable[T](f: Nat => T) = {
    tuningParam(rise.core.freshName.apply("tp"), RangeUnknown, f)
  }

//  val test2 = tunable(splitJoin)

  val test = tunable(splitJoin)

  println("scal: " + scal)

  // rewrite expression
  val sjtp = somebu(splitJoinTP)(RiseTraversable)
  val rewritten1 = sjtp.apply(scal)
  val parameters1 = autotune.constraints.collectParameters(rewritten1.get)
  val constraints1 = autotune.constraints.collectConstraints(rewritten1.get, parameters1)

  println("rewritten1: " + rewritten1)
  println("parameters1: " + parameters1)
  println("constraints1: " + constraints1)

  println("\n")

  // rewrite expression again
  val rewritten2 = sjtp.apply(rewritten1.get)
  val parameters2 = autotune.constraints.collectParameters(rewritten2.get)
  val constraints2 = autotune.constraints.collectConstraints(rewritten2.get, parameters2)

  println("rewritten2: " + rewritten2)
  println("parameters2: " + parameters2)
  println("constraints2: " + constraints2)

  // application with FAILURE
  val rewrittenFailure = splitJoinTP.apply(scal)
  println("rewrittenFailure: " + rewrittenFailure)


  // wrap ocl run

  // lower expressions

  // gen code


//  val codegenStart = System.currentTimeMillis()
//  val m = autoTuningUtils.runWithTimeout(
//    timeouts.codegenerationTimeout)(gen.opencl.hosted("fun").fromExpr(expression)
//  )


  // different lowering strategies
  // strategies introducing tuning parameters
  // map -> mapSeq (all to mapSeq)
  // map, (map, ...) -> MapGlb, (mapSeq, mapSeq, ...) (first mapGlb, rest mapSeq)
  // map, map, (map, ...) -> MapLcl, MapWrg, mapSeq (first mapLcl, second mapWrg, rest mapSeq)

  def main(args: Array[String]): Unit = {
    // start exploration here

    //    riseExploration(scal, "exploration/configuration/scal.json")
  }
}
