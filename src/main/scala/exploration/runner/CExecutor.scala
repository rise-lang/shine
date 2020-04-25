package exploration.runner

import java.io.{File, FileOutputStream, PrintWriter}

import elevate.rise.Rise
import elevate.heuristic_search.Runner
import shine.C.Program
import util.gen
import elevate.core.Strategy
import elevate.heuristic_search.util.IOHelper
import exploration.util.ExecuteC

class CExecutor(val lowering: Strategy[Rise], val goldExpression: Rise, val iterations: Int, val inputSize: Int, val threshold: Double, val output: String) extends Runner[Rise] {
  val N = inputSize
  var best:Option[Double] = None
  var gold = gen.CProgram(goldExpression, "compute_gold")
  var counter = 0

  // write header to csv output file
  writeHeader(output + "/" + "executor.csv")

  def execute(solution: Rise):(Rise,Option[Double]) = {
    // lower solution
    val lowered = lowering.apply(solution)

    //generate executable program (including host code)
    val code = genExecutableCode(lowered.get)

    //compile and execute program
    val performanceValue = compileAndExecute(lowered.get, code, iterations)

    // print code to file
    var codeOutput = ""

    // add high/low-level hash, performance value and code
    codeOutput += "// high-level hash: " + solution.hashCode() + " \n"
    codeOutput += "// low-level hash: " + lowered.get.hashCode() + " \n"

    // check if execution was valid
    var filenameC = solution.hashCode().toString + "_" + lowered.get.hashCode().toString()
    var filenameLowered = lowered.get.hashCode().toString

    performanceValue match {
      case None => {
        codeOutput += "// runtime: " + -1 + "\n \n"
        filenameC += "_error"
        filenameLowered += "_error"
      }
      case _ => codeOutput += "// runtime: " + performanceValue.get.toString  + "\n \n"
    }

    codeOutput += code

    // print code to file
    val uniqueFilenameCode = IOHelper.getUniqueFilename(output + "/C/" + filenameC + ".c", 2)

    // create file for code
    val pwCode = new PrintWriter(new FileOutputStream(new File(uniqueFilenameCode), false))

    // write code to file
    pwCode.write(codeOutput)

    // close files
    pwCode.close()

    // write runtime to output file
    writeValues(output + "/" + "executor.csv", (solution, lowered.get, performanceValue), "executor")

    // print lowered expression to file
    val uniqueFilenameLowered = IOHelper.getUniqueFilename(output + "/lowered/" + filenameLowered, 0)

    // create file for for lowered expression
    val pwLowered = new PrintWriter(new FileOutputStream(new File(uniqueFilenameLowered), false))

    // lowered string
    var loweredString = "high-level hash: " + solution.hashCode() + "\n"
    loweredString += lowered.get

    // write code to file
    pwLowered.write(loweredString)

    // close files
    pwLowered.close()


    // new gold
//    gold = gen.CProgram(goldExpression, "compute_gold")

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

  def compileAndExecute(solution:Rise, code:String, iterations:Int):Option[Double] = {
    try {
      val returnValue = ExecuteC(code, iterations, threshold)

      // check for new best and new gold to use
      best match {
        case Some(value) => {
          if(returnValue.toDouble < value){
            best = Some(returnValue.toDouble)
            gold = gen.CProgram(solution, "compute_gold")
            println("use new gold with runtime: " + best.get)
          }
        }
        case _ => best = Some(returnValue.toDouble)
      }

      println("result: " + returnValue)
      Some(returnValue.toDouble)
    }catch {
      case e: Throwable => {
        println("execution failed")
        None
      }
    }
  }

  def writeValues(path: String, result: (Rise, Rise, Option[Double]), name:String) {
    // open file for appendix
    val file = new PrintWriter(new FileOutputStream(new File(path), true))

    // create string to write to file
    var string = counter + ", " + name + ", " + System.currentTimeMillis().toString + ", " + result._1.hashCode().toString + ", " + result._2.hashCode().toString() + ", "
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
    // open file for appendix
    val file = new PrintWriter(new FileOutputStream(new File(path), true))

    // create string to write to file
    val string = "iteration, " + "runner, " + "timestamp, " + "high-level hash, " + "low-level hash, " + "runtime\n"

    // write to file and close
    file.write(string)
    file.close()
  }


}
