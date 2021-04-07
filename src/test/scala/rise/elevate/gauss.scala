package rise.elevate

import java.io.File

import elevate.core._
import elevate.core.strategies.basic.{normalize}
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

import _root_.util.gen.c.function
import scala.language.postfixOps
import scala.sys.process._
import scala.util.Random

// scalastyle:off
class gauss extends test_util.Tests {

  val outermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.outermost(default.RiseTraversable)
  val innermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.innermost(default.RiseTraversable)

  //// MM INPUT EXPRESSION /////////////////////////////////////////////////////

  val gaussWeights: Seq[Seq[Int]] = Seq(
    Seq(1, 4, 6, 4,1),
    Seq(4,16,24,16,4),
    Seq(6,24,36,24,6),
    Seq(4,16,24,16,4),
    Seq(1, 4, 6, 4,1)
  )

  val N = 512
  val M = 512

  val mulPair = fun(pair => fst(pair) * snd(pair))

  val gauss: Rise = {
    fun(ArrayType(N, ArrayType(M, int)))(in =>
      fun(ArrayType(5, ArrayType(5, int)))(weights =>
        in |> padClamp2D(2) // in: NxM -> (N+4) x (M+4)
          |> slide2D(5, 1) // -> MxN of 5x5 slides
          |> map(map(fun(sector => // sector:5x5
            zip(sector |> join)(weights |> join) |> map(mulPair) |> reduce(add)(l(0)) |> fun(x => x/l(256)) //TODO Halide does a >> 8
        )))
      )
    )
  }
  val lowering = DFNF() `;` lowerToC

  // -- CPU ---------------------------------------------------------------

  val cpu: Strategy[Rise] = DFNF()(default.RiseTraversable) `;`
    normalize.apply(fuseReduceMap `@` topDown[Rise])

  test("CPU") {
    run("cpu", cpu)
  }

  val cpuPar: Strategy[Rise] = cpu `;`
    vectorize(64) `@` bottomUp[Rise]

    //(parallel()    `@` outermost(isApplied(isMap))) `;;`
    /*splitStrategy(1024)   `@` innermost(isApplied(isApplied(isApplied(isReduce)))) `;;`
    reorder(List(1,3,4,2)) `;;`
    vectorize(64) `@` innermost(isApplied(isApplied(isMap)))
    */

  test("CPU par") {
    run("CPU par", cpuPar)
  }

  val splitReduce = splitStrategy(1024)   `@` innermost(isApplied(isApplied(isApplied(isReduce))))
  val reordering = reorder(List(1,2))
  val vectorization = vectorize(64) `@` innermost(isApplied(isApplied(isMap)))
  test("vectorize") {
    val splitted = splitReduce.apply(gauss)
    val reordered = reordering.apply(splitted.get)
    //val vectorized = vectorization.apply(reordered.get)

    val loweredDefault = (cpu `;` lowerToC).apply(gauss)
    val loweredSplitted = (cpu `;` lowerToC).apply(splitted.get)
    val loweredReordered = (cpu `;` lowerToC).apply(reordered.get)
    //val loweredVectorized = (cpu `;` lowerToC).apply(vectorized.get)

    val codeDefault = gen.openmp.function("gaussian").asStringFromExpr(loweredDefault.get)
    //    val codeSplitted = gen.openmp.function("gaussian").asStringFromExpr(loweredSplitted.get) // not valid
    val codeReordered = gen.openmp.function("gaussian").asStringFromExpr(loweredReordered.get)
//    val codeVectorized = gen.openmp.function("gaussian").asStringFromExpr(loweredVectorized.get)

    // print results
    println("gauss: \n" + gauss)
    println("splitted: \n" + splitted.get)
    println("reordered: \n" + reordered.get)
//    println("vectorized: \n" + vectorized.get)

    println("loweredGauss: \n" + loweredDefault.get)
    println("loweredSplitted: \n " + loweredSplitted.get)
    println("loweredReordered: \n" + loweredReordered.get)
//    println("loweredVectorized: \n" + loweredVectorized.get)

    println("code: \n" + codeDefault)
    println("code: \n" + codeReordered)
//    println("code: \n" + codeVectorized)
  }

/*
  test("Executor test"){
    val executor = new CExecutor(cpu, 1, List((N,N),(5,5)), (N,N))
    executor.setInputs(List(
      CExecutor.randomMatrix(N,N),
      gaussWeights
    ))

    executor.genCode(gauss)
  }
*/

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
    val reordering = reorder(List(1,2,3,4))
    val vectorization = vectorize(64) `@` innermost(isApplied(isApplied(isMap)))

    // apply strategies
    val splitted = splitReduce.apply(mm)
    val reordered = reordering.apply(splitted.get)
    //val vectorized = vectorization.apply(reordered.get)

    // lowering
    val loweredDefault = (cpu `;` lowerToC).apply(mm)
    val loweredSplitted = (cpu `;` lowerToC).apply(splitted.get)
    val loweredReordered = (cpu `;` lowerToC).apply(reordered.get)
    //val loweredVectorized = (cpu `;` lowerToC).apply(vectorized.get)

    // code generation
    val codeDefault = gen.openmp.function("mm").asStringFromExpr(loweredDefault.get)
//    val codeSplitted = gen.openmp.function("gaussian").asStringFromExpr(loweredSplitted.get) // not valid
    val codeReordered = gen.openmp.function("mm").asStringFromExpr(loweredReordered.get)
    //val codeVectorized = gen.openmp.function("mm").asStringFromExpr(loweredVectorized.get)

    // print results
    println("mm: \n" + mm)
    println("splitted: \n" + splitted.get)
    println("reordered: \n" + reordered.get)
    //println("vectorized: \n" + vectorized.get)

    println("loweredMM: \n" + loweredDefault.get)
    println("loweredSplitted: \n " + loweredSplitted.get)
    println("loweredReordered: \n" + loweredReordered.get)
    //println("loweredVectorized: \n" + loweredVectorized.get)

    println("code: \n" + codeDefault)
    println("code: \n" + codeReordered)
    //println("code: \n" + codeVectorized)
  }


  /// UTILS ////////////////////////////////////////////////////////////////////
  def run(version: String, strategy: Strategy[Rise]): Unit ={

    def writeToFile(path: String, name: String, content: String, ending: String = ".c"): Unit = {
      import java.io._
      val baseDir = new File(path)
      if(!baseDir.exists()) baseDir.mkdirs()
      val srcFile = new File(s"$path/$name$ending")
      val w = new PrintWriter(srcFile)
      w.write(content)
      w.flush()
      w.close()
    }

    def randomMatrix(dims: (Int, Int)): Seq[Seq[Int]] = {
      Seq.fill(dims._1)(Seq.fill(dims._2)(Random.nextInt()))
    }

    def genMatrixCode(m: Seq[Seq[Int]]): String = {
      "{\n"+m.map(_.mkString(",")).mkString(",\n")+"\n}"
    }


    def genCode(strategy: Strategy[Rise], pgm: Rise): String = {
      val lowered = (strategy`;` lowerToC).apply(gauss)
      val p = gen.openmp.function("gaussian").fromExpr(lowered.get)


      val code = s"""
      #include <stdio.h>
      #include <stdlib.h>
      #include <time.h>

     ${function.asString(p)}

      int main(void){
        int in[] = ${genMatrixCode(randomMatrix((N,M)))}; //TODO read data from file (image) use lib?
        int weights[] = ${genMatrixCode(gaussWeights)};
        int out[${N*M}];

        //measure time
        struct timespec tp_start;
        struct timespec tp_end;
        clockid_t clk_id = CLOCK_MONOTONIC;
        double duration = 0;

        clock_gettime(clk_id, &tp_start);
        gaussian(out, in, weights);
        clock_gettime(clk_id, &tp_end);

        duration = (tp_end.tv_sec - tp_start.tv_sec) * 1000000000 + (tp_end.tv_nsec - tp_start.tv_nsec);
        duration = duration / 1000000;

        //print result
        printf("%f\\n", duration);

        return 0;
      }
      """

      code
    }

/*
    def compile(code: java.io.File, bin: java.io.File): Unit =  {
      s"gcc -Wall -o ${bin.getAbsolutePath} ${code.getAbsolutePath}" !!
    }
*/
    def compile(path: String, name: String, ending: String = ".c"): Unit = {
      val srcFile = new File(s"$path/$name$ending")
      val binFile = new File(s"$path/$name")

      s"gcc -Wall -o ${binFile.getAbsolutePath} ${srcFile.getAbsolutePath}" !!

    }
  def run(path: String, name:String, times: Int): Unit = {
    var runtimes: List[Double] = List.empty
    for(i <- 1 to times){
      try {
        runtimes = (s"${new File(s"$path/$name").getAbsolutePath}" !!).toDouble :: runtimes
      }
      catch {
        case e: Exception => println(e)
      }
    }
    println("Med time " + runtimes.sorted.apply(runtimes.size/2))
  }
    println(version + ":")

    val code = genCode(strategy, gauss)
    writeToFile("./exploration/gaussian", version, code)
    compile("./exploration/gaussian", version)
    run("./exploration/gaussian", version, 10)
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
