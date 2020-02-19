package exploration.search

import elevate.rise.Rise
import util.{Execute2, gen}

object executeC {

  def apply(riseProgram:Rise): Double = {

    //generate executable program (including host code)
    val code = genExecutableCode(riseProgram)

    //compile and execute program
    val performanceValue = compileAndExecute(code)

    performanceValue
  }

  def genExecutableCode(riseProgram:Rise):String = {
    val p = gen.CProgram(riseProgram)

    val testCode = s"""
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
${p.code}

int main(int argc, char** argv) {
  const int N = 1024;

  float input[N];
  float output[N];
  float alpha = 10;
  for (int i = 0; i < N; i++) {
    input[i] = i;
    output[i] = 0;
  }

  //measure time
 	struct timespec tp_start;
	struct timespec tp_end;
	clockid_t clk_id = CLOCK_MONOTONIC;
  double duration = 0;

  clock_gettime(clk_id, &tp_start);
  ${p.function.name}(output, N, input, alpha);
  clock_gettime(clk_id, &tp_end);

  duration = (tp_end.tv_sec - tp_start.tv_sec) * 1000000000 + (tp_end.tv_nsec - tp_start.tv_nsec);
  duration = duration / 1000000;

  for (int i = 0; i < N-2; i++) {
    int expected = input[i]*alpha;

    if (output[i] != expected) {
      fprintf(stderr, "found %f, expected %i\\n", output[i], expected);
      duration = - 1;
    }
  }

  printf("%f", duration);

  return duration;
}
"""

    testCode
  }

  def compileAndExecute(code:String):Double = {
    try {
      val returnValue = Execute2(code)
      println("result: " + returnValue)
      returnValue.toDouble
    }catch {
      case e: Throwable => println("execution failed")
        -1
    }
  }


}
