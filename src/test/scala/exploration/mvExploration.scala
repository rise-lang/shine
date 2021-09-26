package exploration

import apps.gemv.ocl.gemvKeplerBest
import apps.separableConvolution2D
import apps.separableConvolution2D.mulT
import arithexpr.arithmetic.RangeMul
import elevate.core.strategies.traversal.{allTopdown, bottomUp, topDown, tryAll}
import exploration.strategies.scalStrategies
import rise.autotune.{tuningParam, wrapOclRun}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._
import rise.elevate.rules.algorithmic.{fuseReduceMap, splitJoin}
import rise.elevate.rules.lowering.{addRequiredCopies, reduceOCL}
import rise.elevate.rules.traversal.default.RiseTraversable
import rise.elevate.tunable
import rise.openCL.DSL.toGlobal
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen


object mvExploration {
  // we can use implicit type parameters and type annotations to specify the function type of mult
  val mult = impl{ dt: DataType => fun(x => x._1 * x._2) :: ((dt x dt) ->: dt) }
  val add = fun(x => fun(y => x + y))
  val scal = impl { n: Nat =>
    fun(xs => fun(a =>
      map(fun(x => a * x))(xs)
    )) :: (ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
  }

  val dot: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduce(add)(lf32(0.0f))
  ))
  val dotSeq: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduce(add)(lf32(0.0f))
  ))

  val mvHighLevel = depFun((n: Nat, m: Nat) => fun(
    (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32)
  )((mat, xs) =>
    mat |> map(fun(row =>
      zip(row)(xs) |> map(mulT) |> reduce(add)(lf32(0.0f))
    ))
  ))

  println("mvHighLevel: " + mvHighLevel)

  // lower mv high-level
  def lowerGs = {
    topDown(fuseReduceMap) `;` topDown(rise.elevate.rules.lowering.mapGlobal(0)) `;` addRequiredCopies() `;` rise.elevate.rules.lowering.specializeSeq() `;` reduceOCL()

    //normalize(reduceSeq -> reduceOCLSeq private/local/global )
  }

  val lowered = lowerGs.apply(mvHighLevel)
  val fused = topDown(fuseReduceMap).apply(mvHighLevel)
  println("lowered: " + lowered)
  println("fused: " + fused)


  val mvNoTuning = wrapOclRun(LocalSize(32), GlobalSize(1024))(lowered.get)
  println("mvNoTuning: " + mvNoTuning)

  val codeNoTuning = gen.opencl.hosted("fun").fromExpr(mvNoTuning)
  val codeString = gen.opencl.hosted.asString(codeNoTuning)
  println("codeNoTuning: " + codeNoTuning)
  println("codeString: " + codeString)


  // lowering jey
  // try to execute MV

  // try to tune mv
  // automatic lover

  def main(args: Array[String]): Unit = {
    // start exploration here

    // add strategies as arguments
//    riseExploration(mvHighLevel, scalStrategies.lowering, scalStrategies.strategies, "exploration/configuration/scal.json")
  }
}
