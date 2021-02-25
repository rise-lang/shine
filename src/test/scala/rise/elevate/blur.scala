package rise.elevate

import _root_.util.gen
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.debug.debug
import elevate.core.strategies.traversal._
import rise.core.DSL.HighLevelConstructs.{padClamp2D, slide2D}
import rise.core.DSL.{fun, l}
import rise.core.primitives._
import rise.core.types._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering.{lowerToC, parallel, vectorize}
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate.{isApplied, isMap}
import rise.elevate.strategies.traversal
import rise.elevate.strategies.traversal._

// scalastyle:off
class blur extends test_util.Tests {

  val outermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.outermost(default.RiseTraversable)
  val innermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.innermost(default.RiseTraversable)

  //// MM INPUT EXPRESSION /////////////////////////////////////////////////////

  val N = 8
  val M = 8

  val mulPair = fun(pair => fst(pair) * snd(pair))

  val blur: Rise = {
    fun(ArrayType(N, ArrayType(M, f64)))(in =>
      in |> padClamp2D(1) // in: NxM -> (N+2) x (M+2)
        |> slide2D(3, 1) // -> MxNx3x3
        |> map(map(fun(sector=>
          sector |> join |> reduce(add)(l(0.0)) |> fun(x => x/l(9.0))
      )))
    )
  }

  // -- CPU ---------------------------------------------------------------

  val cpu: Strategy[Rise] = DFNF()(default.RiseTraversable) `;`
    normalize.apply(fuseReduceMap `@` topDown[Rise])

  test("CPU seq") {
    println("blur: " + blur)

    val base = cpu.apply(blur)
    println("base: " + base)

    val lowered = lowerToC.apply(base.get)
    println("lowered: " + lowered)

    val code = gen.openmp.function("gaussian").asStringFromExpr(lowered.get)
    println("code: " + code)
  }

  val isFullyAppliedMap: Strategy[Rise] = isApplied(isApplied(isMap))
  val cpuPar: Strategy[Rise] = cpu `;;`
    (parallel()    `@` outermost(isApplied(isMap))) `;;`
    debug[Rise]("after par")

  test("CPU par") {
    run("CPU par", cpuPar)
  }



  /// UTILS ////////////////////////////////////////////////////////////////////

  def run(version: String, strategy: Strategy[Rise]): Unit ={
    println(version + ":")

    val lowered = (strategy`;` lowerToC)(blur)
    println("lowered: " + lowered)

    val code = gen.openmp.function("blur").asStringFromExpr(lowered.get)
    println("code: " + code)
  }

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
//    val rewritten = strategy(blur)
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
//      gen.openmp.function(version).asStringFromExpr(rewritten.get)
//    } else {
//      gen.c.function(version).asStringFromExpr(rewritten.get)
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



