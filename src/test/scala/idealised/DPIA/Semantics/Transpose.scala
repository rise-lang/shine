package idealised.DPIA.Semantics

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.{Execute, SyntaxChecker}

class Transpose extends idealised.util.Tests {
  ignore("Simple transpose should produce the expected result on a test") {
    def checkResult[T <: idealised.SurfaceLanguage.Types.Type]
    (e: idealised.SurfaceLanguage.Expr) =
    {
      val p = idealised.C.ProgramGenerator.makeCode(TypeInference(e, Map()).toPhrase)
      SyntaxChecker(p.code)
      println(p.code)

      val testCode = s"""
#include <stdio.h>

${p.code}

int main(int argc, char** argv) {
  const int N = 6;
  const int M = 10;

  int input[N * M];
  for (int i = 0; i < (N * M); i++) { input[i] = i; }

  int output[M * N];
  ${p.function.name}(output, N, M, input);

  for (int i = 0; i < M; i++) {
    for (int j = 0; j < N; j++) {
      int o = output[i * N + j];
      int expected = j * M + i;

      if (o != expected) {
        fprintf(stderr, "(%i, %i)\\n", i, j);
        fprintf(stderr, "found %i, expected %i\\n", o, expected);
        return 1;
      }
    }
  }

  return 0;
}
"""
      Execute(testCode)
    }
    val gatherExp = nFun(n => nFun(m => fun(ArrayType(n, ArrayType(m, int)))(a =>
      a :>> transpose() :>> mapSeq(mapSeq(fun(x => x)))
    )))
    val scatterExp = nFun(n => nFun(m => fun(ArrayType(n, ArrayType(m, int)))(a =>
      a :>> mapSeq(mapSeq(fun(x => x))) :>> transpose()
    )))
    checkResult(gatherExp)
    checkResult(scatterExp)
  }
}