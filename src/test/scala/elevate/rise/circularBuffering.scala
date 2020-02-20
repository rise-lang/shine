package elevate.rise

import rise.core.types._
import rise.core.primitives.SlideSeq
import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.HighLevelConstructs.dropLast

// import elevate.core.strategies.basic._
// import elevate.core.strategies.traversal._
// import elevate.rise.rules.lowering
// import elevate.rise.rules.traversal._

import util.gen

class circularBuffering extends shine.test_util.Tests {
  private val sum = reduce(add)(l(0.0f))
  private val sumSeq = reduceSeq(add)(l(0.0f))

  test("simple circular buffering example") {
    def wrapExpr(e: Rise): Rise = {
      infer(nFun(n => fun(
        ((n+5)`.`f32) ->: (2`.`n`.`f32)
      )(input =>
        e(input)
      )))
    }

    val highLevel =
      slide(3)(1) >> map(sum) >> fun(x =>
        makeArray(2)(
          x |> slide(2)(1) >> map(sum) >> drop(1) >> dropLast(1),
          x |> slide(4)(1) >> map(sum)
        ))

    println(wrapExpr(highLevel).t)

    val inlined =
      slide(3)(1) >> map(sumSeq) >> fun(x =>
        makeArray(2)(
          x |> slide(2)(1) >> map(sumSeq) >> drop(1) >> dropLast(1),
          x |> slide(4)(1) >> map(sumSeq)
        )) >> mapSeqUnroll(mapSeq(fun(x => x)))

    val buffered =
      slide(3)(1) >> mapSeq(sumSeq) >> let(fun(x =>
        makeArray(2)(
          x |> slide(2)(1) >> map(sumSeq) >> drop(1) >> dropLast(1),
          x |> slide(4)(1) >> map(sumSeq)
        )) >> mapSeqUnroll(mapSeq(fun(x => x))))

    val circBuf =
      slide(3)(1) >> map(sumSeq) >>
        slideSeq(SlideSeq.Indices)(4)(1)(fun(x => x))(fun(nbh =>
          makeArray(2)(
            nbh |> drop(1) >> dropLast(1) >> sumSeq,
            nbh |> sumSeq
          ) |> mapSeqUnroll(fun(x => x))
        )) >> transpose

    val inlinedP = gen.CProgram(wrapExpr(inlined), "inlined")
    val bufferedP = gen.CProgram(wrapExpr(buffered), "buffered")
    val circBufP = gen.CProgram(wrapExpr(circBuf), "circularBuffered")

    val N = 20
    val testCode =
      s"""
        |#include <stdio.h>
        |
        |${inlinedP.code}
        |${bufferedP.code}
        |${circBufP.code}
        |
        |int main(int argc, char** argv) {
        |  float input[$N+5];
        |
        |  for (int i = 0; i < $N+5; i++) {
        |    input[i] = (2 * i + 133) % 19;
        |  }
        |
        |  float output1[2*$N];
        |  inlined(output1, $N, input);
        |
        |  float output2[2*$N];
        |  buffered(output2, $N, input);
        |
        |  float output3[2*$N];
        |  circularBuffered(output3, $N, input);
        |
        |  for (int i = 0; i < 2*$N; i++) {
        |    if (
        |      (output1[i] != output2[i]) ||
        |      (output1[i] != output3[i])
        |    ) {
        |      fprintf(stderr, "(%f, %f, %f)\\n",
        |        output1[i], output2[i], output3[i]);
        |      return 1;
        |    }
        |  }
        |
        |  return 0;
        |}
        |""".stripMargin
    util.Execute(testCode)
  }
}
