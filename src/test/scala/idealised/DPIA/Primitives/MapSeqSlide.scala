package idealised.DPIA.Primitives

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.NatIdentifier
import idealised.SurfaceLanguage.Types._
import idealised.util.{Execute, SyntaxChecker}

class MapSeqSlide extends idealised.util.Tests {
  test("Simple example should generate C code producing the expected result on a test") {
    val e = dFun((n: NatIdentifier) =>fun(ArrayType(n, int))(a =>
      a :>> mapSeqSlide(3, 1, reduceSeq(fun(_ + _), 0))))
    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(e, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)

    val testCode = s"""
#include <stdio.h>

$code

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
