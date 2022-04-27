package exploration.runner

import elevate.core.Strategy
import elevate.heuristic_search.Runner
import elevate.heuristic_search.util.{IOHelper, Solution}
import exploration.explorationUtil.ExplorationErrorLevel.{ExplorationErrorLevel, _}
import rise.elevate.Rise
import shine.C
import shine.C.AST.ParamDecl
import util.gen.c.function
import util.{createTempFile, gen, writeToTempFile}

import java.io.{File, FileOutputStream, PrintWriter}
import scala.language.postfixOps
import scala.sys.process._

case class CExecutor(lowering: Strategy[Rise],
                     goldExpression: Rise,
                     iterations: Int,
                     inputSize: Int,
                     threshold: Double,
                     output: String) extends Runner[Rise] {
  var globalBest:Option[Double] = None
  val N: Int = inputSize
  var best:Option[Double] = None
  var gold: C.Module = gen.openmp.function("compute_gold").fromExpr(goldExpression)
  var counter = 0
  var errorLevel:ExplorationErrorLevel = LoweringError

  // write header to csv output file
  writeHeader(output + "/" + "executor.csv")

  def plot(): Unit = {

  }

  override def checkSolution(solution: Solution[Rise]): Boolean = {

    true
  }

  def execute(solution: Solution[Rise]):(Rise,Option[Double]) = {
    println("[Executor] : strategy length: " + solution.strategies.size)
    solution.strategies.foreach(elem => {
      println("strategy: " + elem)
    })
    // initialize error level
    errorLevel = LoweringError

    // lower solution
    val lowered = lowering.apply(solution.expression)

    // update error level
    errorLevel = CodeGenerationError

    //generate executable program (including host code)
    var performanceValue:Option[Double] = None
    var code = ""
    try {
      code = genExecutableCode(lowered.get)

      errorLevel = CompilationError

      // compile
      try {
        val bin = compile(code)

        // execute
        try{
          errorLevel = ExecutionError
          println("execute: " + Integer.toHexString(solution.expression.hashCode()))
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
            // handle different execution errors
            e.getMessage.substring(20).toInt match {
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
                throw new Exception("segmentation fault")
              case _ =>
                throw new Exception("unknow error code")
            }
        }
      } catch{
        case _: Throwable =>
          println("compiling error")
      }

    } catch {
      case _: Throwable =>
        println("code gen error")
        code = "code generation error "
    }

    // result is performance value

    //compile and execute program
    // val performanceValue = compileAndExecute(lowered.get, code, iterations)

    // print code to file
    var codeOutput = ""

    // add high/low-level hash, performance value and code
    codeOutput += "// high-level hash: " + Integer.toHexString(solution.expression.hashCode()) + " \n"
    codeOutput += "// low-level hash: " + Integer.toHexString(lowered.get.hashCode()) + " \n"

    // check if execution was valid
    var filenameC = Integer.toHexString(solution.expression.hashCode()) + "_" + Integer.toHexString(lowered.get.hashCode())
    var filenameLowered = Integer.toHexString(lowered.get.hashCode())
    var filenameHigh = Integer.toHexString(solution.expression.hashCode())
    var folder = output + "/" + Integer.toHexString(solution.expression.hashCode())

    performanceValue match {
      case None =>
        codeOutput += "// runtime: " + -1 + "\n \n"
        //        filenameC += "_error"
        filenameC += "_" + errorLevel.toString
        filenameLowered += "_" + errorLevel.toString
        filenameHigh += "_" + errorLevel.toString
        folder += "_" + errorLevel.toString
      case _ => codeOutput += "// runtime: " + performanceValue.get.toString  + "\n \n"
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
    writeValues(output + "/" + "executor.csv", (solution.expression, lowered.get, performanceValue, errorLevel), "executor")

    // print lowered expression to file
    val uniqueFilenameLowered = IOHelper.getUniqueFilename(folder + "/" + filenameLowered, 0)

    // create file for for lowered expression
    val pwLowered = new PrintWriter(new FileOutputStream(new File(uniqueFilenameLowered), false))

    // lowered string
    var loweredString = "high-level hash: " + Integer.toHexString(solution.expression.hashCode()) + "\n"
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
    val pwStrategy= new PrintWriter(new FileOutputStream(new File(uniqueFilenameStrategies), false))

    // create strategy string
    var strategyString = ""
    solution.strategies.foreach(elem => {
      strategyString += s"$elem\n"
    })

    // write and close
    pwStrategy.write(strategyString)
    pwStrategy.close()

    (solution.expression, performanceValue)
  }

  def prepareInput(tu: C.Module):(String,String,String,String) = {

    val fun: C.AST.Function = tu.functions.head

    val arrayTwo = "(.)+[.](.)+[.]f32".r
    val arrayOne = "(.)+[.]f32".r
    val elemOne = "f32".r

    var codeBeg = s"""
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

    fun.inputParams.foreach{ case (decl, meta) =>

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

    codeBeg += s"""

        //output"""

    if(outputParamDecl.t.toString.equals("int")) {
      codeBeg +=
        s"""
        const int ${outputParamDecl.name} = N; """
    } else if (arrayTwo.findFirstIn(outputParamMeta.typ.toString).isDefined) {
      codeBeg += s"""
        float* ${outputParamDecl.name} = (float*) malloc(sizeof(float)*N*N);
        float* gold = (float*) malloc(sizeof(float)*N*N);
        for (int i = 0; i < N*N; i++) {
          ${outputParamDecl.name}[i] = 0;
          gold[i] = 0;
        }
        """
      codeEnd += s"""
        free(gold);
        free(${outputParamDecl.name});"""
    } else if (arrayOne.findFirstIn(outputParamMeta.typ.toString).isDefined) {
      codeBeg += s"""
        float* ${outputParamDecl.name} = (float*) malloc(sizeof(float)*N);
        float* gold = (float*) malloc(sizeof(float)*N);
        for (int i = 0; i < N; i++) {
          ${outputParamDecl.name}[i] = 0;
          gold[i] = 0;
        }
        """
      codeEnd += s"""
        free(gold);
        free(${outputParamDecl.name});"""

    } else if (elemOne.findFirstIn(outputParamMeta.typ.toString).isDefined) {
      codeBeg += s"""
        float ${outputParamDecl.name} = N;
        float gold = N;
        """
    }

    call += s""");"""
    callGold += s""");"""

    (codeBeg,codeEnd, call, callGold)
  }

  // maybe change name
  def prepareGold(): String ={

    val arrayTwo = "(.)+[.](.)+[.]f32".r
    val arrayOne = "(.)+[.]f32".r
    val elemOne = "f32".r

    var codeBeg = s"""
int compare_gold(float* C, float* GOLD){
  int valid = 1;"""

    val goldOutputParam = gold.functions.head.outputParams.head._1

    if(goldOutputParam.t.toString.equals("int")) {
      throw new Exception("Should not reach this point")
    } else if (arrayTwo.findFirstIn(goldOutputParam.t.toString).isDefined) {
      codeBeg += s"""
        for(int i = 0; i < SIZE*SIZE; i++){
    		  if(C[i] != GOLD[i]){
    			  valid = 0;
            i = SIZE*SIZE;
    		  }
    	  }
        """
    } else if (arrayOne.findFirstIn(goldOutputParam.t.toString).isDefined) {
      codeBeg += s"""
        for(int i = 0; i < SIZE; i++){
          if(C[i] != GOLD[i]){
    			  valid = 0;
            i = SIZE*SIZE;
    		  }
    	  }
        """
    } else if (elemOne.findFirstIn(goldOutputParam.t.toString).isDefined) {
      codeBeg += s"""
        if(C[0] != GOLD[0]){
    			  valid = 0;
        }
        """
    }

    codeBeg += s"""
    return valid;
    }
    """

    codeBeg
  }

  def genExecutableCode(riseProgram:Rise):String = {
    val p = gen.openmp.function("riseFun").fromExpr(riseProgram)

    val preparation = prepareInput(p)

    val goldCheck = prepareGold()

    val testCode = s"""
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
    s"clang -O2 $src -o $bin -lm -fopenmp" !!

    bin
  }

  def execute(bin: String, iterations: Int, threshold: Double): String = {
    //repeat execution
    //take median as runtime
    val N = iterations
    val runtimes:Array[Double] = new Array[Double](N)
    var runtime = 0.0
    //check global execution time. Discard any with factor 10
    var i = 0
    while(i < N) {
      runtimes(i) = (s"$bin" !!).toDouble

      println("runtime:(" + i +"): " + runtimes(i))
      println("globalBest: " + globalBest)
      // check if we have to skip this execution round
      globalBest match{
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
    runtime = runtimes.sorted.apply(N/2)

    // check if new global best was found
    if (runtime < globalBest.get) {
      globalBest = Some(runtime)
    }

    runtime.toString
  }


  def writeValues(path: String,
                  result: (Rise, Rise, Option[Double], ExplorationErrorLevel),
                  name:String): Unit = {
    // open file to append values
    val file = new PrintWriter(
      new FileOutputStream(new File(path), true))

    // create string to write to file
    var string = s"$counter, $name, ${System.currentTimeMillis().toString}, " +
      s"${Integer.toHexString(result._1.hashCode())}, " +
      s"${Integer.toHexString(result._2.hashCode())}, ${result._4.toString}, "
    result._3 match {
      case Some(value) => string += value.toString + "\n"
      case _ => string += "-1 \n"
    }

    // write to file and close
    file.write(string)
    counter += 1
    file.close()
  }

  def writeHeader(path:String): Unit = {
    // open file
    val file = new PrintWriter(
      new FileOutputStream(new File(path), false))

    // create string to write to file
    val string = "iteration, runner, timestamp, high-level hash, " +
      "low-level hash, error-level, runtime\n"

    // write to file and close
    file.write(string)
    file.close()
  }


}
