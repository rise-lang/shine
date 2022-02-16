package rise.elevate

import _root_.util.gen.c.function
import _root_.util.{gen, writeToTempFile}
import elevate.core._
import elevate.core.strategies.traversal._
import rise.core.DSL.HighLevelConstructs.{padClamp2D, padCst2D, slide2D}
import rise.core.DSL.Type.ArrayTypeConstructorsFromInt
import rise.core.DSL.{ToBeTyped, fun, l, lf32}
import rise.core.Lambda
import rise.core.primitives.{reduce, _}
import rise.core.types.DataType.f32
import rise.core.types._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering.lowerToC
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.halide.reorder
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.traversal
import rise.elevate.strategies.traversal._

import java.io.{File, FileInputStream, FileOutputStream}
import scala.language.postfixOps
import scala.sys.process._
import scala.util.Random

// scalastyle:off
class tvmConvolution extends test_util.Tests {
  val outermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.outermost(default.RiseTraversable)
  val innermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.innermost(default.RiseTraversable)

  //// MM INPUT EXPRESSION /////////////////////////////////////////////////////

  val gaussWeights: Seq[Seq[Number]] = Seq(
    Seq(1, 4, 6, 4,1),
    Seq(4,16,24,16,4),
    Seq(6,24,36,24,6),
    Seq(4,16,24,16,4),
    Seq(1, 4, 6, 4,1)
  )


  val batch = 256
  val in_channel = 256
  val out_channel = 512
  val in_size = 14
  val kernel = 3
  val pad = 1

  val mulPair: ToBeTyped[Lambda] = fun(pair => fst(pair) * snd(pair))


  val mAddM: Rise = fun(a =>
    fun(b =>
      zip(a)(b) |> map(fun(rowPair =>
        zip(fst(rowPair))(snd(rowPair)) |> map(fun(valPair =>
          rise.core.DSL.Ops(fst(valPair)) + snd(valPair) // explicitly use rise.core.DSL.Ops, otherwise Scala will confuse rise.core.DSL.Ops + with String concatenation...
        ))
      ))
    )
  )


  val convolution: Rise = {
    fun(in_size `.` in_size `.` in_channel `.` batch `.` f32)(A =>
      fun(kernel `.` kernel `.` in_channel `.` out_channel `.` f32)(W =>
        A //|> padCst2D(pad)(ArrayType(in_channel,ArrayType(batch,f32)))

          //|> padClamp2D(pad) //placeholder for something like padClamp2D(1,in_channel x batch array of f32 0)
          |> slide2D(kernel,1)
          |> map(map(fun(
            sector => zip(sector |> join |> join)(W |> join |> join) |> map(fun(
                pair => snd(pair) |> map(fun(
                  ff => fst(pair) |> map(fun(
                    nn => nn * ff
                  ))
                ))
              ))
              //|> reduce(mAddM)(vectorFromScalar(vectorFromScalar(lf32(0.0f))))

              //Aaaaand finally, reorder so the reduction axis is innermost, then reduce.
              |> transpose |> map(fun(
                outer => outer |> transpose |> map(fun(x => x)) |> map(
                  reduce(add)(lf32(0))
                ) //|> map(fun(x => x))
            )) //map(map(fun( x => x)))
          )))
      )
    )
  }
  /*
  val gauss: Rise = {
    fun(ArrayType(N, ArrayType(M, int)))(in =>
      fun(ArrayType(5, ArrayType(5, int)))(weights =>
        in |> padClamp2D(2) // in: NxM -> (N+4) x (M+4)
          |> slide2D(5, 1) // -> MxN of 5x5 slides
          |> map(map(fun(sector => // sector:5x5
            zip(sector |> join)(weights |> join) |> map(mulPair) |> reduce(add)(l(0)) |> fun(x => x/l(256))
        )))
      )
    )
  }
   */


  val lowering = DFNF() `;` lowerToC

  // -- CPU ---------------------------------------------------------------

  val cpu: Strategy[Rise] = DFNF()(default.RiseTraversable) //`;`
    fuseReduceMap `@` topDown[Rise]


  test("convolution") {
    run("cpu", convolution, cpu)
  }


    /*
    val cpuPar: Strategy[Rise] = cpu `;;`

      //(vectorize(32) `@` innermost(isApplied(isMap)))  `;;`
      (parallel()    `@` outermost(isApplied(isMap)))

      //parallel() //`;`
      //(vectorize(64) `@` bottomUp[Rise])

      //(parallel()    `@` outermost(isApplied(isMap))) `;;`
      /*splitStrategy(1024)   `@` innermost(isApplied(isApplied(isApplied(isReduce)))) `;;`
      reorder(List(1,3,4,2)) `;;`
      vectorize(64) `@` innermost(isApplied(isApplied(isMap)))
      */
  */
  test("CPU reorder") {
    /*
    LCNFrewrite(
      gauss,
      body(body(reorder(Seq(2,1)))) //`@` innermost(isApplied(isApplied(isApplied(isMap))))
    )
    //println(LCNFrewrite(gauss, body(body(reorder(Seq(1,3,2))))))
    //run("CPU par", gauss, cpuPar)
     */
  }

  /*
  val splitReduce = splitStrategy(1024)   `@` innermost(isApplied(isApplied(isApplied(isReduce))))
  //val reordering = reorder(List(1,2))
  val vectorization = vectorize(64) `@` innermost(isApplied(isApplied(isMap)))
  test("vectorize") {
    val splitted = splitReduce.apply(gauss)
    val reordered = reordering.apply(splitted.get)
    val vectorized = vectorization.apply(reordered.get)

    val loweredDefault = (cpu `;` lowerToC).apply(gauss)
    val loweredSplitted = (cpu `;` lowerToC).apply(splitted.get)
    val loweredReordered = (cpu `;` lowerToC).apply(reordered.get)
//    val loweredVectorized = (cpu `;` lowerToC).apply(vectorized.get)

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
*/

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

  /// UTILS ////////////////////////////////////////////////////////////////////
  def run(version: String, expresion:Rise, strategy: Strategy[Rise]): Unit ={
    println(version + ":")

    val lowered = (strategy`;` lowerToC)(expresion)
    println("lowered: " + lowered.get)

    val c = gen.openmp.function("gaussian").asStringFromExpr(lowered.get)
    println("code: " + c)


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

    def randomMatrix(generator: () => Number, dims: (Int, Int)): Seq[Seq[Number]] = {
      Seq.fill(dims._1)(Seq.fill(dims._2)(generator()))
    }

    def genMatrixFileCode(m: Seq[Seq[Number]]): String = {
      m.map(_.mkString(",")).mkString("\n")
    }

    def genMatrixCode(m: Seq[Seq[Number]]): String = {
      "{\n"+m.map(_.mkString(",")).mkString(",\n")+"\n}"
    }


    /*
    def genCode(strategy: Strategy[Rise], pgm: Rise): String = {
      val lowered = (strategy`;` lowerToC).apply(convolution)
      val p = gen.openmp.function("gaussian").fromExpr(lowered.get)


      s"""
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

${function.asString(p)}

int main(void){
  int* in = calloc(${N*M}, sizeof(int));
  int weights[] = ${genMatrixCode(gaussWeights)};
  int* out = calloc(${N*M}, sizeof(int));


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
}"""
    }
     */
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

  def generateInputs(expected: Seq[Either[(()=>Number, Int, Int), Seq[Seq[Number]]]]): Seq[Seq[Seq[Number]]] = {
    expected.map {
      case Left(generateTriple) => randomMatrix(generateTriple._1, (generateTriple._2, generateTriple._3))
      case Right(fixedData) => fixedData
    }
  }

  def run(path: String, name: String, times: Int, data: Seq[Either[(()=>Number, Int, Int), Seq[Seq[Number]]]]): Unit = {

    var runtimes: List[Double] = List.empty
    for(_ <- 1 to times){

      val inputs = generateInputs(data)
      val inputFiles = inputs.map(m=>genMatrixFileCode(m)).map(data=>writeToTempFile("GaussInput","",data))
      val persistentFiles = inputFiles.map(f=> {
        val dest = new File("./exploration/gaussian/"+f.getName)
        new FileOutputStream(dest).getChannel.transferFrom(new FileInputStream(f).getChannel,0, Long.MaxValue)
        dest
      })

      println(persistentFiles)

      try {
        runtimes = (s"${new File(s"$path/$name").getAbsolutePath}" !!).toDouble :: runtimes
      }
      catch {
        case e: Exception => println("Could not run Kernel: "+e)
      }
    }
    println("Med time " + runtimes.sorted.apply(runtimes.size/2))
  }
    println(version + ":")

    /*
    val code = genCode(strategy, convolution)
    writeToFile("./exploration/gaussian", version, code)
    compile("./exploration/gaussian", version)
    run(
      "./exploration/gaussian",
      version,
      10,
      Seq(Left((()=>Random.nextInt(256), N, M)), Right(gaussWeights))
    )
     */
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
