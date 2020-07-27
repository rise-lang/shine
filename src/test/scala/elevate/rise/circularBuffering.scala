package elevate.rise

import rise.core.types._
import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.HighLevelConstructs.dropLast
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.rise.rules._
import elevate.rise.rules.traversal.alternative
import elevate.rise.rules.traversal.alternative._
import elevate.rise.rules.algorithmic._
import elevate.rise.rules.movement._
import elevate.rise.strategies.predicate._
import elevate.rise.rules.lowering

import util.gen
import elevate.util.makeClosed

class circularBuffering extends shine.test_util.Tests {
  private val sum = reduce(add)(l(0.0f))
  private val sumSeq = reduceSeq(add)(l(0.0f))

  def wrapExpr(e: Rise): Rise = {
    infer(nFun(n => fun(
      ((n+5)`.`f32) ->: (2`.`n`.`f32)
    )(input =>
      e(input)
    )))
  }

  val highLevel: Rise =
    slide(3)(1) >> map(sum) >> fun(x =>
      makeArray(2)(
        x |> slide(2)(1) >> map(sum) >> drop(1) >> dropLast(1),
        x |> slide(4)(1) >> map(sum)
      ))

  val inlined: Rise =
    slide(3)(1) >> map(sumSeq) >> fun(x =>
      makeArray(2)(
        x |> slide(2)(1) >> map(sumSeq) >> drop(1) >> dropLast(1),
        x |> slide(4)(1) >> map(sumSeq)
      )) >> mapSeqUnroll(mapSeq(fun(x => x)))

  val buffered: Rise =
    slide(3)(1) >> mapSeq(sumSeq) >> toMem >> letf(x =>
      makeArray(2)(
        x |> slide(2)(1) >> map(sumSeq) >> drop(1) >> dropLast(1),
        x |> slide(4)(1) >> map(sumSeq)
      )) >> mapSeqUnroll(mapSeq(fun(x => x)))

  val circBuf: Rise =
    slide(3)(1) >> map(sumSeq) >>
    circularBuffer(4)(4)(fun(x => x)) >>
    mapStream(fun(nbh =>
      makeArray(2)(
        nbh |> drop(1) >> dropLast(1) >> sumSeq,
        nbh |> sumSeq
      ) |> mapSeqUnroll(fun(x => x))
    )) >> transpose

  test("example outputs are consistent") {
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

  private def openEquality(typedA: Rise, b: Rise): Boolean = {
    import rise.core.TypedDSL._
    val typedB: Rise = toTDSL(b) :: typedA.t
    makeClosed(typedA)._1 == makeClosed(typedB)._1
  }

  private val norm = normalize(alternative.RiseTraversable).apply(gentleBetaReduction())

  private def rewriteSteps(a: Rise, steps: Seq[(Strategy[Rise], Rise)]): Unit = {
    steps.foldLeft[Rise](norm(infer(a)).get)({ case (e, (s, expected)) =>
      val result = (s `;` norm)(e).get
      val nuExpect = norm(infer(expected)).get
      if (!openEquality(result, nuExpect)) {
        throw new Exception(
          s"expected structural equality.\nGot:\n$result\nExpected:\n$nuExpect")
      }
      result
    })
  }

  test("highLevel to circBuf") {
    rewriteSteps(highLevel, Seq(
      (topDown(dropBeforeMap)`;` topDown(takeBeforeMap))
      -> (
        slide(3)(1) >> map(sum) >> fun(x =>
        makeArray(2)(
          x |> slide(2)(1) >> drop(1) >> dropLast(1) >> map(sum),
          x |> slide(4)(1) >> map(sum)
        ))
      ),
      (topDown(dropInSlide) `;` topDown(takeBeforeMap) `;` topDown(takeInSlide))
      -> (
      slide(3)(1) >> map(sum) >> fun(x =>
        makeArray(2)(
          x |> slide(4)(1) >> map(dropLast(1)) >> map(drop(1)) >> map(sum),
          x |> slide(4)(1) >> map(sum)
        ))
      ),
      normalize.apply(mapFusion)
      -> (
      slide(3)(1) >> map(sum) >> fun(x =>
        makeArray(2)(
          x |> slide(4)(1) >> map(dropLast(1) >> drop(1) >> sum),
          x |> slide(4)(1) >> map(sum)
        ))
      ),
      topDown(mapOutsideMakeArray)
      -> (
      slide(3)(1) >> map(sum) >> slide(4)(1) >> map(fun(x =>
        makeArray(2)(
          x |> dropLast(1) >> drop(1) >> sum,
          x |> sum
        ))) >> transpose
      ),
      (normalize.apply(lowering.reduceSeq) `;`
        topDown(dropBeforeTake) `;`
        topDown(isApply `;` one(isApply `;` one(isMakeArray)) `;`
          lowering.mapSeqUnrollWrite) `;`
        topDown(lowering.circularBuffer(fun(x => x))) `;`
        topDown(lowering.mapStream))
      -> circBuf,
    ))
  }
}
