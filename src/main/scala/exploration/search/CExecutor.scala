package exploration.search

import elevate.rise.Rise
import elevate.core.Strategy
import elevate.heuristic_search.Runner
import shine.C.Program
import util.{Execute2, gen}

class CExecutor(val lowering: Strategy[Rise], val goldExpression: Rise, val iterations: Int, val inputSize: Int) extends Runner[Rise] {
  val N = inputSize
  var best:Option[Double] = None
  var gold = gen.CProgram(goldExpression, "compute_gold")

  def execute(solution: Rise):(Rise,Option[Double]) = {
    // execute C code here

    // lower solution
    val lowered = lowering.apply(solution)

    //generate executable program (including host code)
    val code = genExecutableCode(lowered.get)

//    println("code: "  + code)

    //compile and execute program
    val performanceValue = compileAndExecute(lowered.get, code, iterations)

    // new gold
    gold = gen.CProgram(goldExpression, "compute_gold")


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
      val returnValue = Execute2(code, iterations)

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
}
