package rise.elevate

import elevate.core._
import elevate.core.strategies.basic._
import rise.elevate.rules.lowering.lowerToC
import _root_.util.gen
import elevate.core.strategies.traversal._
import rise.core.DSL.HighLevelConstructs.{padClamp2D, slide2D, zipND}
import rise.core.DSL.{fun, l}
import rise.core.primitives._
import rise.core.types._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.traversal
import rise.elevate.strategies.traversal._

// scalastyle:off
class gauss extends test_util.Tests {

  val outermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.outermost(default.RiseTraversable)
  val innermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.innermost(default.RiseTraversable)

  //// MM INPUT EXPRESSION /////////////////////////////////////////////////////

  val gaussWeights = List(
    List(2,4,5,4,2),
    List(4,9,12,9,4),
    List(5,12,15,12,5),
    List(4,9,12,9,4),
    List(2,4,5,4,2)
  )

  val N = 1024
  val M = 1024

  val zip2D = zipND(2)
  val mulPair = fun(pair => fst(pair) * snd(pair))

  val gauss: Rise = { //infer(
    fun(ArrayType(N, ArrayType(M, f32)))(in =>
      fun(ArrayType(5, ArrayType(5, f32)))(weights =>
        in |> padClamp2D(2) // in: NxM -> (N+4) x (M+4)
          |> slide2D(5,2) // -> MxN of 5x5 slides
          |> map(map(fun(sector => // sector:5x5
            zip2D(sector, weights) |> join |> map(mulPair) |> reduce(add)(l(0.0))
        )))
      )
    )
/*
  fun(ArrayType(N, ArrayType(M, f32)))(in =>
    fun(ArrayType(5, ArrayType(5, f32)))(weights =>
      in |> padClamp2D(2) // in: NxM -> (N+4) x (M+4)
        |> slide2D(5,2) // -> MxN of 5x5 slides
        |> map(map(fun(sector => // sector:5x5
        zip(sector)(weights) //(v,w)
          |> map(fun(rowPair =>
          zip(fst(rowPair))(snd(rowPair))
            |> map(mulPair) |> reduce(add)(l(0.0f))
        )) |> reduce(add)(l(0.0f))
      )))
    )
  )
*/

  // -- BASELINE ---------------------------------------------------------------

  val baseline: Strategy[Rise] = DFNF()(default.RiseTraversable) `;`
    normalize.apply(fuseReduceMap `@` topDown[Rise])

  test("baseline") {
    println("gauss: " + gauss)

    val lowered = lowerToC.apply(gauss)
    println("lowered: " + lowered)

    val code = gen.openmp.function("riseFun").fromExpr(lowered.get)
    println("code: " + code)
  }

  /// UTILS ////////////////////////////////////////////////////////////////////

//  def run(version: String,
//          strategy: Strategy[Rise],
//          openMP: Boolean // generate C or OpenMP code?
//         ): Unit = {
//
//    val generateFiles = false
//    val kernelsFolder: String = "/home/artifact/kernels"
//    val plotsFolder: String = "/home/artifact/results/fig10/steps"
//
//    def writeToFile(path: String, name: String, content: String, ending: String = ".c"): Unit = {
//      import java.io._
//      val w =new PrintWriter(new File(s"$path/$name$ending"))
//      w.write(content)
//      w.flush()
//      w.close()
//    }
//
//    def currentTimeSec: Long = System.currentTimeMillis / 1000
//
//    val versionUC = version.toUpperCase()
//    // reset rewrite step counter
//    Success.rewriteCount = 0
//
//    // rewrite the matmul input expresssion
//    val time0 = currentTimeSec
//    val rewritten = strategy(gauss)
//    val time1 = currentTimeSec
//    println(s"[$versionUC] rewrite time: ${time1 - time0}s")
//    if (generateFiles) {
//      val steps = Success.rewriteCount
//      println(s"[$versionUC] required rewrite steps: $steps\n")
//      writeToFile(plotsFolder, version, s"$version,$steps", ".csv")
//    }
//
//    // generate the C code
//    val time2 = currentTimeSec
//    val program = if(openMP) {
////      gen.OpenMPProgram(rewritten.get, version).code
//    } else {
////      gen.CProgram(rewritten.get, version).code
//    }
//    val time3 = currentTimeSec
//    println(s"[$versionUC] codegen time: ${time3 - time2}s")
//    println(s"Program:\n${program}")
//
//    // store the C code
//    if (generateFiles) {
//      println(s"[$versionUC] generated code stored as $version in $kernelsFolder")
//      writeToFile(kernelsFolder, version, program)
//    }
//  }
}
