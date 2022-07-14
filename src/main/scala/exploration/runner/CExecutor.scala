package exploration.runner

import elevate.core.Strategy
import elevate.heuristic_search.Runner
import elevate.heuristic_search.util.{IOHelper, Solution, hashProgram}
import exploration.explorationUtil.ExplorationErrorLevel
import rise.elevate.Rise
import shine.C
import shine.C.AST.ParamDecl
import util.gen.c.function
import util.{createTempFile, gen, writeToTempFile}
import exploration.explorationUtil.ExplorationErrorLevel._
import exploration.runner
import elevate.heuristic_search.ExplorationResult

import java.io.{File, FileOutputStream, PrintWriter}
import scala.language.postfixOps
import scala.sys.process._

// todo some things can cause problems
// global best -> used for threshold (e.g. change of input size can cause problems)
// gold is replaced if faster valid version was found
// -> will fail if another expression is used -> update executor first
case class CExecutor(
                      lowering: Strategy[Rise],
                      goldExpression: Rise,
                      iterations: Int = 10,
                      inputSize: Int,
                      threshold: Double = 1000.0,
                      output: String = "exploration",
                      timeout: Double = 10000,
                      saveToDisk: Boolean = true
                    ) extends Runner[Rise] {

  var globalBest: Option[Double] = None
  val N: Int = inputSize
  var best: Option[Double] = None
  var gold: C.Module = gen.openmp.function("compute_gold").fromExpr(goldExpression)
  var counter = 0
  var errorLevel: ExplorationErrorLevel = LoweringError
  var samples = 0

  // write header to csv output file
  writeHeader(output + "/" + "executor.csv")

  // todo implement this based on debug executor implementation
  def plot(): Unit = {

    // also write config file
    val doe = counter

    val configString = {
      s"""{
      "application_name": "mm_exploration",
      "optimization_objectives": ["runtime"],
      "feasible_output" : {
        "enable_feasible_predictor" : true,
        "name" : "Valid",
        "true_value" : "True",
        "false_value" : "False"
      },
      "hypermapper_mode" : {
        "mode" : "client-server"
      },
      "design_of_experiment": {
        "doe_type": "random sampling",
        "number_of_samples": ${doe}
      },
      "optimization_iterations": 0,
      "input_parameters" : {
        "index": {
        "parameter_type" : "integer",
        "values" : [0, ${doe}],
        "dependencies" : [],
        "constraints" : []
      }
      }
    }"""
    }

    // write configstring
    val configFilePath = output + "/" + "tuningStatistics.json"
    val fWriter = new PrintWriter(new FileOutputStream(new File(configFilePath), true))
    fWriter.write(configString)
    fWriter.close()

    // plot results
    //    val outputFilePath = output + "/" + "tuningStatistics_hm.csv"

    output + "/" + "executor.csv"

    // mkdir output folder
    (s"mkdir -p ${output}/hm " !!)
    (s"mv ${output}/executor_hm.csv ${output}/hm" !!)


    try {
      // call plot
      val command = s"hm-plot-optimization-results -j ${configFilePath} -i ${output}/hm -l exploration -o ${output}/plot.pdf --y_label 'Log Runtime(ms)' --title exploration"
      val command2 = s"hm-plot-optimization-results -j ${configFilePath} -i ${output}/hm -l exploration -o ${output}/plot.pdf --y_label 'Log Runtime(ms)' --title exploration"
      println("plot: " + command)
      (command !!)
      println("plotlog: " + command2)
      (command2 !!)
    } catch {
      case e: Throwable => // ignore
    }
  }

  def checkSolution(solution: Solution[Rise]): Boolean = {
    runner.checkSolutionC(lowering, solution)
  }

  //  override def plot(): Unit = ???

  def execute(solution: Solution[Rise]): ExplorationResult[Rise] = {
    //    println("[Executor] : strategy length: " + solution.strategies.size)
    //    solution.strategies.foreach(elem => {
    //      println("strategy: " + elem)
    //    })
    // initialize error level
    errorLevel = LoweringError

    // lower solution
    val lowered = lowering.apply(solution.expression)

    // update error level
    errorLevel = CodeGenerationError

    //generate executable program (including host code)
    var performanceValue: Option[Double] = None
    var code = ""
    try {
      code = genExecutableCode(lowered.get)

      errorLevel = CompilationError

      // compile
      try {
        val bin = compile(code)

        // execute
        try {
          errorLevel = ExecutionError
          println("execute: " + hashProgram(solution.expression))
          val returnValue = execute(bin, iterations, threshold)

          // check for new best to replace gold
          best match {
            case Some(value) =>
              if (returnValue.toDouble < value) {
                best = Some(returnValue.toDouble)
                gold = gen.openmp.function("compute_gold").fromExpr(lowered.get)
                println("use new gold with runtime: " + best.get)
              }
            case _ => best = Some(returnValue.toDouble)
          }

          println("result: " + returnValue)
          performanceValue = Some(returnValue.toDouble)
          errorLevel = ExecutionSuccess

        } catch {
          case e: Throwable =>
            println("error handling")
            println("e: " + e)
            // handle different execution errors
            e.getMessage.substring(20).toInt match {
              case 124 =>
                println("timeout")
                errorLevel = ExecutionTimeout
                performanceValue = None
              case 11 =>
                println("execution crashed")
                System.exit(-1)
                errorLevel = ExecutionError
                performanceValue = None
              case 255 =>
                println("execution failed")
                errorLevel = ExecutionFail
                performanceValue = None
              case 139 =>
                println("execution failed with segmentation fault")
                errorLevel = ExecutionFail
                performanceValue = None
              case _ =>
                println("execution failed with unknown error")
                errorLevel = ExecutionFail
                performanceValue = None
            }
        }
      } catch {
        case e: Throwable =>
          println("compiling error: " + e)
      }

    } catch {
      case e: Throwable =>
        println("code gen error")
        println("e: " + e)
        code = "code generation error "
    }

    // result is performance value

    //compile and execute program
    // val performanceValue = compileAndExecute(lowered.get, code, iterations)

    saveToDisk match {
      case true =>

        var codeOutput = ""
        // add high/low-level hash, performance value and code
        codeOutput += "// high-level hash: " + hashProgram(solution.expression) + " \n"
        codeOutput += "// low-level hash: " + hashProgram(lowered.get) + " \n"

        // check if execution was valid
        var filenameC = hashProgram(solution.expression) + "_" + hashProgram(lowered.get)
        var filenameLowered = hashProgram(lowered.get)
        var filenameHigh = hashProgram(solution.expression)
        var folder = output + "/" + hashProgram(solution.expression)

        performanceValue match {
          case None =>
            codeOutput += "// runtime: " + -1 + "\n \n"
            //        filenameC += "_error"
            filenameC += "_" + errorLevel.toString
            filenameLowered += "_" + errorLevel.toString
            filenameHigh += "_" + errorLevel.toString
            folder += "_" + errorLevel.toString
          case _ => codeOutput += "// runtime: " + performanceValue.get.toString + "\n \n"
        }

        // create folder for high-level expression
        folder = IOHelper.getUniqueFilename(folder, 0)
        s"mkdir $folder" !!

        codeOutput += code

        // print code to file
        val uniqueFilenameCode = IOHelper.getUniqueFilename(folder + "/" + filenameC + ".c", 2)

        // create file for code
        val pwCode = new PrintWriter(new FileOutputStream(new File(uniqueFilenameCode), false))

        // write code to file
        pwCode.write(codeOutput)

        // close files
        pwCode.close()

        // write lowered expressions

        // write runtime to output file
        writeValues(output + "/" + "executor.csv", (solution.expression, lowered.get, performanceValue, errorLevel), solution.strategies, "executor")

        // print lowered expression to file
        val uniqueFilenameLowered = IOHelper.getUniqueFilename(folder + "/" + filenameLowered, 0)

        // create file for for lowered expression
        val pwLowered = new PrintWriter(new FileOutputStream(new File(uniqueFilenameLowered), false))

        // lowered string
        var loweredString = "high-level hash: " + hashProgram(solution.expression) + "\n"
        loweredString += lowered.get

        // write code to file
        pwLowered.write(loweredString)

        // close files
        pwLowered.close()

        // write high-level expressions

        // print lowered expression to file
        val uniqueFilenameHigh = IOHelper.getUniqueFilename(folder + "/" + filenameHigh, 0)

        // create file for for lowered expression
        val pwHigh = new PrintWriter(new FileOutputStream(new File(uniqueFilenameHigh), false))

        // write code to file
        pwHigh.write(solution.expression.toString)

        // close files
        pwHigh.close()

        // write strategies
        val uniqueFilenameStrategies = IOHelper.getUniqueFilename(folder + "/" + filenameHigh + "_strategies", 0)

        // create file for for lowered expression
        val pwStrategy = new PrintWriter(new FileOutputStream(new File(uniqueFilenameStrategies), false))

        // create strategy string
        var strategyString = ""
        solution.strategies.foreach(elem => {
          strategyString += s"$elem\n"
        })

        // write and close
        pwStrategy.write(strategyString)
        pwStrategy.close()

      case false => // nothing
    }

    ExplorationResult[Rise](
      solution,
      performanceValue,
      None
    )
  }

  def prepareInput(tu: C.Module): (String, String, String, String) = {

    val fun: C.AST.Function = tu.functions.head

    val arrayTwo = "(.)+[.](.)+[.]f32".r
    val arrayOne = "(.)+[.]f32".r
    val elemOne = "f32".r

    var codeBeg =
      s"""
        const int N = $N;
        """

    var codeEnd =
      s"""
       //free memory"""

    var call = s"""${fun.name}(output"""
    var callGold = s"""${gold.functions.head.name}(gold"""

    codeBeg +=
      s"""
        //inputs""".stripMargin

    fun.inputParams.foreach { case (decl, meta) =>

      //      println("elem: " + decl)
      //      println("type: " + decl.t)

      if (decl.t.toString.equals("int")) {
        codeBeg +=
          s"""
        const int ${decl.name} = N; """
        call += s""", ${decl.name}"""
        callGold += s""", ${decl.name}"""
      } else if (arrayTwo.findFirstIn(meta.typ.toString).isDefined) {
        codeBeg +=
          s"""
        float* ${decl.name} = (float*) malloc(sizeof(float)*N*N);
        for (int i = 0; i < N*N; i++) {
          ${decl.name}[i] = (rand() % 100) - 50;
          //${decl.name}[i] = i;
        }
        """
        codeEnd +=
          s"""
        free(${decl.name});"""
        call += s""", ${decl.name}"""
        callGold += s""", ${decl.name}"""
      } else if (arrayOne.findFirstIn(meta.typ.toString).isDefined) {
        codeBeg +=
          s"""
        float* ${decl.name} = (float*) malloc(sizeof(float)*N);
        for (int i = 0; i < N; i++) {
          ${decl.name}[i] = (rand() % 100) - 50;
          //${decl.name}[i] = i;
        }
        """
        codeEnd +=
          s"""
        free(${decl.name});"""
        call += s""", ${decl.name}"""
        callGold += s""", ${decl.name}"""
      } else if (elemOne.findFirstIn(decl.t.toString).isDefined) {
        codeBeg +=
          s"""
        float ${decl.name} = 5;
        """
        call += s""", ${decl.name}"""
        callGold += s""", ${decl.name}"""
      }
    }

    val outputParamDecl = fun.outputParams.head._1
    val outputParamMeta = fun.outputParams.head._2

    codeBeg +=
      s"""

        //output"""

    if (outputParamDecl.t.toString.equals("int")) {
      codeBeg +=
        s"""
        const int ${outputParamDecl.name} = N; """
    } else if (arrayTwo.findFirstIn(outputParamMeta.typ.toString).isDefined) {
      codeBeg +=
        s"""
        float* ${outputParamDecl.name} = (float*) malloc(sizeof(float)*N*N);
        float* gold = (float*) malloc(sizeof(float)*N*N);
        for (int i = 0; i < N*N; i++) {
          ${outputParamDecl.name}[i] = 0;
          gold[i] = 0;
        }
        """
      codeEnd +=
        s"""
        free(gold);
        free(${outputParamDecl.name});"""
    } else if (arrayOne.findFirstIn(outputParamMeta.typ.toString).isDefined) {
      codeBeg +=
        s"""
        float* ${outputParamDecl.name} = (float*) malloc(sizeof(float)*N);
        float* gold = (float*) malloc(sizeof(float)*N);
        for (int i = 0; i < N; i++) {
          ${outputParamDecl.name}[i] = 0;
          gold[i] = 0;
        }
        """
      codeEnd +=
        s"""
        free(gold);
        free(${outputParamDecl.name});"""

    } else if (elemOne.findFirstIn(outputParamMeta.typ.toString).isDefined) {
      codeBeg +=
        s"""
        float ${outputParamDecl.name} = N;
        float gold = N;
        """
    }

    call += s""");"""
    callGold += s""");"""

    (codeBeg, codeEnd, call, callGold)
  }

  // maybe change name
  def prepareGold(): String = {

    val arrayTwo = "(.)+[.](.)+[.]f32".r
    val arrayOne = "(.)+[.]f32".r
    val elemOne = "f32".r

    var codeBeg =
      s"""
int compare_gold(float* C, float* GOLD){
  int valid = 1;"""

    val goldOutputParam = gold.functions.head.outputParams.head._1

    if (goldOutputParam.t.toString.equals("int")) {
      throw new Exception("Should not reach this point")
    } else if (arrayTwo.findFirstIn(goldOutputParam.t.toString).isDefined) {
      codeBeg +=
        s"""
        for(int i = 0; i < SIZE*SIZE; i++){
    		  if(C[i] != GOLD[i]){
    			  valid = 0;
            i = SIZE*SIZE;
    		  }
    	  }
        """
    } else if (arrayOne.findFirstIn(goldOutputParam.t.toString).isDefined) {
      codeBeg +=
        s"""
        for(int i = 0; i < SIZE; i++){
          if(C[i] != GOLD[i]){
    			  valid = 0;
            i = SIZE*SIZE;
    		  }
    	  }
        """
    } else if (elemOne.findFirstIn(goldOutputParam.t.toString).isDefined) {
      codeBeg +=
        s"""
        if(C[0] != GOLD[0]){
    			  valid = 0;
        }
        """
    }

    codeBeg +=
      s"""
    return valid;
    }
    """

    codeBeg
  }

  def genExecutableCode(riseProgram: Rise): String = {
    val p = gen.openmp.function("riseFun").fromExpr(riseProgram)

    val preparation = prepareInput(p)

    val goldCheck = prepareGold()

    val testCode =
      s"""
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
int SIZE = $N;
${function.asString(p)}

${function.asString(gold)}

$goldCheck

int main(int argc, char** argv) {

  ${preparation._1}

  //measure time
 	struct timespec tp_start;
	struct timespec tp_end;
	clockid_t clk_id = CLOCK_MONOTONIC;
  double duration = 0;

  clock_gettime(clk_id, &tp_start);
  ${preparation._3}
  clock_gettime(clk_id, &tp_end);

  duration = (tp_end.tv_sec - tp_start.tv_sec) * 1000000000 + (tp_end.tv_nsec - tp_start.tv_nsec);
  duration = duration / 1000000;

  ${preparation._4}
  int check = compare_gold(output, gold);

  ${preparation._2}

  //check result
  if(!check){
    return -1;
  }

  //print result
  printf("%f\\n", duration);

  return 0;
}
"""

    testCode
  }

  def compile(code: String): String = {
    // create files for source code and binary
    val src = writeToTempFile("code-", ".c", code).getAbsolutePath
    val bin = createTempFile("bin-", "").getAbsolutePath

    // todo: make this configable using json file
    // compile
    //        s"clang -O2 $src -o $bin -lm -fopenmp" !!
    //      s"gcc -O2 $src -o $bin -lm -fopenmp" !!

    //    s"clang $src -o $bin -Ofast -ffast-math -fopenmp" !!

    //      s"clang $src -o $bin -lm -fopenmp" !!
    //    s"clang -O2 $src -o $bin -lm -fopenmp" !!
    s"gcc -O2 $src -o $bin -lm -fopenmp" !!
    //    s"clang $src -o $bin -lm -fopenmp" !!
    //    s"gcc $src -o $bin -lm -fopenmp" !!

    bin
  }

  def execute(bin: String, iterations: Int, threshold: Double): String = {
    //repeat execution
    //take median as runtime
    val N = iterations
    val runtimes: Array[Double] = new Array[Double](N)
    var runtime = 0.0
    //check global execution time. Discard any with factor 10
    var i = 0

    while (i < N) {
      //      runtimes(i) = (s"$bin" !!).toDouble

      runtimes(i) = (s"timeout " +
        s"${(timeout * 1).toDouble / 1000.toDouble}s " +
        s"$bin" !!).toDouble

      //      println("runtime:(" + i + "): " + runtimes(i))
      //      println("globalBest: " + globalBest)
      // check if we have to skip this execution round
      globalBest match {
        case Some(value) =>
          if (runtimes(i) > value * threshold) {
            for (j <- Range(i, N)) {
              runtimes(j) = runtimes(i)
            }
            i = N
          }
        case _ => globalBest = Some(runtimes(i))
      }
      i = i + 1
    }

    // get runtime (median of iterations)
    runtime = runtimes.sorted.apply(N / 2)

    // check if new global best was found
    if (runtime < globalBest.get) {
      globalBest = Some(runtime)
    }

    runtime.toString
  }


  def writeValues(path: String,
                  result: (Rise, Rise, Option[Double], ExplorationErrorLevel),
                  rewrite: Seq[Strategy[Rise]],
                  name: String): Unit = {

    //    samples += 1

    // open file to append values
    val file = new PrintWriter(
      new FileOutputStream(new File(path), true))

    val fileHM = new PrintWriter(
      new FileOutputStream(new File(path.substring(0, path.size - 4) + "_hm.csv"), true))

    // create string to write to file
    var string = s"$counter, $name, ${System.currentTimeMillis().toString}, " +
      hashProgram(result._1) + ", " +
      hashProgram(result._2) + ", " +
      rewrite.mkString("\"[", ", ", "]\"") + ", " +
      result._4.toString + ", "

    result._3 match {
      case Some(value) => string += value.toString + "\n"
      case _ => string += "-1 \n"
    }


    val stringHmAppendix = result._3 match {
      case Some(value) => value.toString + "," + "True" + "," + System.currentTimeMillis().toString + "\n"
      case None => "-1" + "," + "False" + "," + System.currentTimeMillis().toString + "\n"
    }

    val stringHm = s"${counter}" + "," + stringHmAppendix

    // write to file and close
    file.write(string)
    fileHM.write(stringHm)

    counter += 1

    file.close()
    fileHM.close()
  }

  def writeHeader(path: String): Unit = {
    // open file
    val file = new PrintWriter(
      new FileOutputStream(new File(path), false))

    val fileHM = new PrintWriter(
      new FileOutputStream(new File(path.substring(0, path.size - 4) + "_hm.csv"), false))

    //    println("hello: " + path.substring(0, path.size - 4) + "_hm.csv")

    // create string to write to file
    val string = "iteration, runner, timestamp, high-level hash, " +
      "low-level hash, rewrite, error-level, runtime\n"

    val stringHM = "index,runtime,Valid,Timestamp" + "\n"

    // write to file and close
    file.write(string)
    fileHM.write(stringHM)

    fileHM.close()
    file.close()
  }

}