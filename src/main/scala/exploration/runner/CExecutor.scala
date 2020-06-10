package exploration.runner

import java.io.{File, FileOutputStream, PrintWriter}

import elevate.rise.Rise
import elevate.heuristic_search.Runner
import shine.C.Program
import util.{createTempFile, gen, writeToTempFile}
import elevate.core.Strategy
import elevate.heuristic_search.util.IOHelper
import exploration.explorationUtil.ExplorationErrorLevel.ExplorationErrorLevel
//import exploration.explorationUtil.executeC
import exploration.explorationUtil.ExplorationErrorLevel._
//import exploration.explorationUtil.executeC.{Exception, globalBest}

import scala.sys.process._
import scala.language.postfixOps

class CExecutor(val lowering: Strategy[Rise], val goldExpression: Rise, val iterations: Int, val inputSize: Int, val threshold: Double, val output: String) extends Runner[Rise] {
  var globalBest:Option[Double] = None
  val N = inputSize
  var best:Option[Double] = None
  var gold = gen.CProgram(goldExpression, "compute_gold")
  var counter = 0
  var errorLevel:ExplorationErrorLevel = LoweringError

  // write header to csv output file
  writeHeader(output + "/" + "executor.csv")

  def execute(solution: Rise):(Rise,Option[Double]) = {
    // initialize error level
    errorLevel = LoweringError

    // lower solution
    val lowered = lowering.apply(solution)

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
          val returnValue = execute(bin, iterations, threshold)

          // check for new best to replace gold
          best match {
            case Some(value) => {
              if (returnValue.toDouble < value) {
                best = Some(returnValue.toDouble)
                gold = gen.CProgram(lowered.get, "compute_gold")
                println("use new gold with runtime: " + best.get)
              }
            }
            case _ => best = Some(returnValue.toDouble)
          }

          println("result: " + returnValue)
          performanceValue = Some(returnValue.toDouble)
          errorLevel = ExecutionSuccess

        } catch {
          case e:Throwable => {
            // handle different execution errors
            e.getMessage.substring(20).toInt match {
              case 11 => {
                println("execution crashed")
                System.exit(-1)
                errorLevel = ExecutionError
                performanceValue = None
              }
              case 255 => {
                println("execution failed")
                errorLevel = ExecutionFail
                performanceValue = None
              }
              case _ => {
                println("unknown error code - end program") // unknown error code end program
                System.exit(-1)
              }
            }
          }
        }
      } catch{
        case e:Throwable => {
          println("compiling error")
        }
      }

    } catch {
      case e:Throwable => {
        println("code gen error")
        code = "code generation error "
      }
    }

    // result is performance value

    //compile and execute program
    // val performanceValue = compileAndExecute(lowered.get, code, iterations)

    // print code to file
    var codeOutput = ""

    // add high/low-level hash, performance value and code
    codeOutput += "// high-level hash: " + Integer.toHexString(solution.hashCode()) + " \n"
    codeOutput += "// low-level hash: " + Integer.toHexString(lowered.get.hashCode()) + " \n"

    // check if execution was valid
    var filenameC = Integer.toHexString(solution.hashCode()) + "_" + Integer.toHexString(lowered.get.hashCode())
    var filenameLowered = Integer.toHexString(lowered.get.hashCode())
    var filenameHigh = Integer.toHexString(solution.hashCode())
    var folder = output + "/" + Integer.toHexString(solution.hashCode())

    performanceValue match {
      case None => {
        codeOutput += "// runtime: " + -1 + "\n \n"
//        filenameC += "_error"
        filenameC += "_" + errorLevel.toString
        filenameLowered += "_" + errorLevel.toString
        filenameHigh += "_" + errorLevel.toString
        folder += "_" + errorLevel.toString
      }
      case _ => codeOutput += "// runtime: " + performanceValue.get.toString  + "\n \n"
    }

    // create folder for high-level expression
    folder = IOHelper.getUniqueFilename(folder, 0)
    (s"mkdir ${folder}" !!)

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
    writeValues(output + "/" + "executor.csv", (solution, lowered.get, performanceValue, errorLevel), "executor")

    // print lowered expression to file
    val uniqueFilenameLowered = IOHelper.getUniqueFilename(folder + "/" + filenameLowered, 0)

    // create file for for lowered expression
    val pwLowered = new PrintWriter(new FileOutputStream(new File(uniqueFilenameLowered), false))

    // lowered string
    var loweredString = "high-level hash: " + Integer.toHexString(solution.hashCode()) + "\n"
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
    pwHigh.write(solution.toString)

    // close files
    pwHigh.close()

    (solution, performanceValue)
  }

  def prepareInput(riseProgram:Program):(String,String,String,String) ={

    val arrayTwo = "(.)+[.](.)+[.]f32".r
    val arrayOne = "(.)+[.]f32".r
    val elemOne = "f32".r

    var codeBeg = s"""
        const int N = ${N};
        """

    var codeEnd =
      s"""
       //free memory"""

    var call = s"""${riseProgram.function.name}(output"""
    var callGold = s"""${gold.function.name}(gold"""

    codeBeg +=
      s"""
        //inputs""".stripMargin
    riseProgram.inputParams.foreach(elem => {
      if (elem.`type`.dataType.toString.equals("int")) {
        codeBeg +=
          s"""
        const int ${elem.name} = N; """
        call += s""", ${elem.name}"""
        callGold += s""", ${elem.name}"""
      } else if (arrayTwo.findFirstIn(elem.`type`.dataType.toString).size > 0) {
        codeBeg +=
          s"""
        float* ${elem.name} = (float*) malloc(sizeof(float)*N*N);
        for (int i = 0; i < N*N; i++) {
          ${elem.name}[i] = i;
        }
        """
        codeEnd +=
          s"""
        free(${elem.name});"""
        call += s""", ${elem.name}"""
        callGold += s""", ${elem.name}"""
      } else if (arrayOne.findFirstIn(elem.`type`.dataType.toString).size > 0) {
        codeBeg +=
          s"""
        float* ${elem.name} = (float*) malloc(sizeof(float)*N);
        for (int i = 0; i < N; i++) {
          ${elem.name}[i] = i;
        }
        """
        codeEnd +=
          s"""
        free(${elem.name});"""
        call += s""", ${elem.name}"""
        callGold += s""", ${elem.name}"""
      } else if (elemOne.findFirstIn(elem.`type`.dataType.toString).size > 0) {
        codeBeg +=
          s"""
        float ${elem.name} = 5;
        """
        call += s""", ${elem.name}"""
        callGold += s""", ${elem.name}"""
      }
    })

    codeBeg += s"""

        //output"""

    if(riseProgram.outputParam.`type`.dataType.toString.equals("int")) {
      codeBeg +=
        s"""
        const int ${riseProgram.outputParam.name} = N; """
    } else if (arrayTwo.findFirstIn(riseProgram.outputParam.`type`.dataType.toString).size > 0) {
      codeBeg += s"""
        float* ${riseProgram.outputParam.name} = (float*) malloc(sizeof(float)*N*N);
        float* gold = (float*) malloc(sizeof(float)*N*N);
        for (int i = 0; i < N*N; i++) {
          ${riseProgram.outputParam.name}[i] = 0;
          gold[i] = 0;
        }
        """
      codeEnd += s"""
        free(gold);
        free(${riseProgram.outputParam.name});"""
    } else if (arrayOne.findFirstIn(riseProgram.outputParam.`type`.dataType.toString).size > 0) {
      codeBeg += s"""
        float* ${riseProgram.outputParam.name} = (float*) malloc(sizeof(float)*N);
        float* gold = (float*) malloc(sizeof(float)*N);
        for (int i = 0; i < N; i++) {
          ${riseProgram.outputParam.name}[i] = 0;
          gold[i] = 0;
        }
        """
      codeEnd += s"""
        free(gold);
        free(${riseProgram.outputParam.name});"""

    } else if (elemOne.findFirstIn(riseProgram.outputParam.`type`.dataType.toString).size > 0) {
      codeBeg += s"""
        float ${riseProgram.outputParam.name} = N;
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

    if(gold.outputParam.`type`.dataType.toString.equals("int")) {
      throw new Exception("Should not reach this point")
      codeBeg += s""""""
    } else if (arrayTwo.findFirstIn(gold.outputParam.`type`.dataType.toString).size > 0) {
      codeBeg += s"""
        for(int i = 0; i < SIZE*SIZE; i++){
    		  if(C[i] != GOLD[i]){
    			  valid = 0;
            i = SIZE*SIZE;
    		  }
    	  }
        """
    } else if (arrayOne.findFirstIn(gold.outputParam.`type`.dataType.toString).size > 0) {
      codeBeg += s"""
        for(int i = 0; i < SIZE; i++){
          if(C[i] != GOLD[i]){
    			  valid = 0;
            i = SIZE*SIZE;
    		  }
    	  }
        """
    } else if (elemOne.findFirstIn(gold.outputParam.`type`.dataType.toString).size > 0) {
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

    val p = gen.CProgram(riseProgram)

    val preparation = prepareInput(p)

    val goldCheck = prepareGold

    val testCode = s"""
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
int SIZE = ${N};
${p.code}

${gold.code}

${goldCheck}

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

//  def compileAndExecute(solution:Rise, code:String, iterations:Int):Option[Double] = {
//    try {
//      val returnValue = executeC(code, iterations, threshold)
//
//      // check for new best and new gold to use
//      best match {
//        case Some(value) => {
//          if(returnValue.toDouble < value){
//            best = Some(returnValue.toDouble)
//            gold = gen.CProgram(solution, "compute_gold")
//            println("use new gold with runtime: " + best.get)
//          }
//        }
//        case _ => best = Some(returnValue.toDouble)
//      }
//
//      println("result: " + returnValue)
//      Some(returnValue.toDouble)
//    }catch {
//      case e: Throwable => {
//        println("execution failed")
//        None
//      }
//    }
//  }

  def compile(code: String): String = {
    // create files for source code and binary
    val src = writeToTempFile("code-", ".c", code).getAbsolutePath
    val bin = createTempFile("bin-", "").getAbsolutePath

    // todo: make this configable using json file
    // compile
    (s"clang -O2 $src -o $bin -lm" !!)

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
        case Some(value) => {
          runtimes(i) > value * threshold match {
            case true => {
              //break up
              for( j <- Range(i, N)){
                runtimes(j) = runtimes(i)
              }
              i = N
            }
            case false => // continue
          }
        }
        case _ => globalBest = Some(runtimes(i))
      }
      i = i + 1
    }

    // get runtime (median of iterations)
    runtime = runtimes.sorted.apply(N/2)

    // check if new global best was found
    runtime < globalBest.get match {
      case true => globalBest = Some(runtime)
      case false =>
    }

    runtime.toString
  }


  def writeValues(path: String, result: (Rise, Rise, Option[Double], ExplorationErrorLevel), name:String) {
    // open file to append values
    val file = new PrintWriter(new FileOutputStream(new File(path), true))

    // create string to write to file
    var string = counter + ", " + name + ", " + System.currentTimeMillis().toString + ", " + Integer.toHexString(result._1.hashCode()) + ", " + Integer.toHexString(result._2.hashCode()) + ", " + result._4.toString + ", "
    result._3 match{
      case Some(value) => string += value.toString + "\n"
      case _ => string += "-1 \n"
    }

    // write to file and close
    file.write(string)
    counter += 1
    file.close()
  }

  def writeHeader(path:String) {
    // open file
    val file = new PrintWriter(new FileOutputStream(new File(path), false))

    // create string to write to file
    val string = "iteration, " + "runner, " + "timestamp, " + "high-level hash, " + "low-level hash, " + "error-level, " + "runtime\n"

    // write to file and close
    file.write(string)
    file.close()
  }


}
