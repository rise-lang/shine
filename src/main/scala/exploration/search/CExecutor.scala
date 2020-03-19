package exploration.search

import elevate.rise.Rise
import elevate.core.Strategy
import elevate.heuristic_search.Runner
import shine.C.Program
import util.{Execute2, gen}

class CExecutor(val lowering: Strategy[Rise], val iterations: Int) extends Runner[Rise] {
  val N = 1024

  def execute(solution: Rise):(Rise,Option[Double]) = {
    // execute C code here

    // lower solution
    val lowered = lowering.apply(solution)

    //generate executable program (including host code)
    val code = genExecutableCode(lowered.get)

    //compile and execute program
    val performanceValue = compileAndExecute(code, iterations)

    (solution, performanceValue)
  }

  def prepareInput(riseProgram:Program):(String,String,String) ={

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

    codeBeg +=
      s"""
        //inputs""".stripMargin
    riseProgram.inputParams.foreach(elem => {
      if (elem.`type`.dataType.toString.equals("int")) {
        codeBeg +=
          s"""
        const int ${elem.name} = N; """
        call += s""", ${elem.name}"""
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
      } else if (elemOne.findFirstIn(elem.`type`.dataType.toString).size > 0) {
        codeBeg +=
          s"""
        float ${elem.name} = 5;
        """
        call += s""", ${elem.name}"""
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

    (codeBeg,codeEnd, call)
  }

  // to be implemented
  def prepareGold(): String ={
//        val code = s"""
//      void compute_gold(float* GOLD, float* A, float* B){
//    	  for(int i = 0; i < SIZE; i++)
//    	  	for(int k = 0; k < SIZE; k++)
//    			  for(int j = 0; j < SIZE; j++)
//    				  GOLD[i*SIZE+j] += A[i*SIZE+k] * B[k*SIZE+j];
//       }
//
////      void compute_gold(float* GOLD, float* A, float B){
////    	  for(int i = 0; i < SIZE; i++)
////    	  	for(int k = 0; k < SIZE; k++)
////    			  for(int j = 0; j < SIZE; j++)
////    				  GOLD[i] += A[i] * B;
////       }
//
//      int compare_gold(float* C, float* GOLD){
//    	  int valid = 1;
//    	  for(int i = 0; i < SIZE*SIZE; i++){
////    	  for(int i = 0; i < SIZE; i++){
//    		  if(C[i] != GOLD[i]){
//    			  valid = 0;
//            i = SIZE*SIZE;
//    		  }
//    	  }
//    	  return valid;
//      }
//      """
    val code = s""""""
    code
  }

  def genExecutableCode(riseProgram:Rise):String = {

    val p = gen.CProgram(riseProgram)

    val preparation = prepareInput(p)

    val gold = prepareGold

    val testCode = s"""
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
int SIZE = ${N};
${p.code}

${gold}

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
//
//// to be implemented
//  compute_gold(gold, x0, x1);
//  int check = compare_gold(output, gold);
//
//  if(!check){
//    return -1;
//  }

  //print result

  ${preparation._2}

  printf("%f\\n", duration);

  return 0;
}
"""

    testCode
  }

  def compileAndExecute(code:String, iterations:Int):Option[Double] = {
    try {
      val returnValue = Execute2(code, iterations)
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
