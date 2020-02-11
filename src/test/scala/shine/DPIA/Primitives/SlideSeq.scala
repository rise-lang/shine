package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.types._
import rise.core.primitives.SlideSeq._
import util.{Execute, gen}

class SlideSeq extends shine.test_util.Tests {
  val add = fun(a => fun(b => a + b))

  test("Simple example should generate C code producing the expected result on a test") {
    val e = nFun(n => fun(ArrayType(n, int))(a =>
      a |> slideSeq(Values)(3)(1)(fun(x => x))(reduceSeq(add)(l(0)))
    ))
    val p = gen.CProgram(e)

    val testCode = s"""
#include <stdio.h>

${p.code}

int main(int argc, char** argv) {
  const int N = 50;

  int input[N];
  for (int i = 0; i < N; i++) { input[i] = i; }

  int output[N];
  ${p.function.name}(output, N, input);

  for (int i = 0; i < N-2; i++) {
    int expected = input[i] + input[i+1] + input[i+2];

    if (output[i] != expected) {
      fprintf(stderr, "found %i, expected %i\\n", output[i], expected);
      return 1;
    }
  }

  return 0;
}
"""
    Execute(testCode)

    println(testCode)
  }
}
