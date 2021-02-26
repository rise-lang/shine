package rise.elevate

import elevate.core._
import elevate.core.strategies.basic._
import rise.elevate.rules.lowering.{lowerToC, parallel, vectorize}
import _root_.util.gen
import elevate.core.strategies.traversal._
import rise.core.DSL.HighLevelConstructs.{padClamp2D, slide2D, zipND}
import rise.core.DSL.{fun, l}
import rise.core.primitives._
import rise.core.types._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate.{isApplied, isMap, isReduce}
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

  val N = 8
  val M = 8

  val mulPair = fun(pair => fst(pair) * snd(pair))

  val gauss: Rise = {
    fun(ArrayType(N, ArrayType(M, f64)))(in =>
      fun(ArrayType(5, ArrayType(5, f64)))(weights =>
        in |> padClamp2D(2) // in: NxM -> (N+4) x (M+4)
          |> slide2D(5, 1) // -> MxN of 5x5 slides
          |> map(map(fun(sector => // sector:5x5
            zip(sector |> join)(weights |> join) |> map(mulPair) |> reduce(add)(l(0.0)) |> fun(x => x/l(159.0))
        )))
      )
    )
  }

  // -- CPU ---------------------------------------------------------------

  val cpu: Strategy[Rise] = DFNF()(default.RiseTraversable) `;`
    normalize.apply(fuseReduceMap `@` topDown[Rise])

  test("CPU") {
    run("cpu", cpu)
  }

  val cpuPar: Strategy[Rise] = cpu `;;`
    (vectorize(32) `@` innermost(isApplied(isMap))) `;;`
    (parallel()    `@` outermost(isApplied(isMap)))

  test("CPU par") {
    run("CPU par", cpuPar)
  }

  test("vectorization example"){

    val N = 1024

    // define algorithm
    val mm: Rise =
      fun(ArrayType(N, ArrayType(N, f32)))(a =>
        fun(ArrayType(N, ArrayType(N, f32)))(b =>
          a |> map(fun(ak =>
            transpose(b) |> map(fun(bk =>
              zip(ak)(bk) |>
                map(fun(x => fst(x) * snd(x))) |>
                reduce(add)(l(0.0f))
            ))
          ))
        ))

    // define strategies
    val splitReduce = splitStrategy(1024)   `@` innermost(isApplied(isApplied(isApplied(isReduce))))
    val reordering = reorder(List(1,3,4,2))
    val vectorization = vectorize(64) `@` innermost(isApplied(isApplied(isMap)))

    // apply strategies
    val splitted = splitReduce.apply(mm)
    val reordered = reordering.apply(splitted.get)
    val vectorized = vectorization.apply(reordered.get)

    // lowering
    val loweredDefault = (cpu `;` lowerToC).apply(mm)
    val loweredSplitted = (cpu `;` lowerToC).apply(splitted.get)
    val loweredReordered = (cpu `;` lowerToC).apply(reordered.get)
    val loweredVectorized = (cpu `;` lowerToC).apply(vectorized.get)

    // code generation
    val codeDefault = gen.openmp.function("mm").asStringFromExpr(loweredDefault.get)
//    val codeSplitted = gen.openmp.function("gaussian").asStringFromExpr(loweredSplitted.get) // not valid
    val codeReordered = gen.openmp.function("mm").asStringFromExpr(loweredReordered.get)
    val codeVectorized = gen.openmp.function("mm").asStringFromExpr(loweredVectorized.get)

    // print results
    println("mm: \n" + mm)
    println("splitted: \n" + splitted.get)
    println("reordered: \n" + reordered.get)
    println("vectorized: \n" + vectorized.get)

    println("loweredMM: \n" + loweredDefault.get)
    println("loweredSplitted: \n " + loweredSplitted.get)
    println("loweredReordered: \n" + loweredReordered.get)
    println("loweredVectorized: \n" + loweredVectorized.get)

    println("code: \n" + codeDefault)
    println("code: \n" + codeReordered)
    println("code: \n" + codeVectorized)
  }


  /// UTILS ////////////////////////////////////////////////////////////////////
  def run(version: String, strategy: Strategy[Rise]): Unit ={
    println(version + ":")

    val lowered = (strategy`;` lowerToC)(gauss)
    println("lowered: " + lowered)

    val code = gen.openmp.function("gaussian").asStringFromExpr(lowered.get)
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
