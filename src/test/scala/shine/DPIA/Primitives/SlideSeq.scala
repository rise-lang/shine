package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import util.gen.c.function
import util.{Execute, gen}

class SlideSeq extends test_util.Tests {
  val add = fun(a => fun(b => a + b))

  def check3pSum(e: rise.core.Expr): Unit = {
    val sumFun = function("sum").asStringFromExpr(e)
    val testCode = s"""
#include <stdio.h>

$sumFun

int main(int argc, char** argv) {
  const int N = 50;

  int input[N];
  for (int i = 0; i < N; i++) { input[i] = i; }

  int output[N];
  sum(output, N, input);

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
  }

  test("3 point sum value rotation:"
    + " generated C code gives the expected result"
  ) {
    check3pSum(depFun((n: Nat) => fun(ArrayType(n, int))(a => a |>
      rotateValues(3)(fun(x => x)) >>
      iterateStream(reduceSeq(add)(l(0)))
    )))
  }

  test("3 point sum index rotation:"
    + " generated C code gives the expected result"
  ) {
    check3pSum(depFun((n: Nat) => fun(ArrayType(n, int))(a => a |>
      circularBuffer(3)(3)(fun(x => x)) >>
      iterateStream(reduceSeq(add)(l(0)))
    )))
  }
}
