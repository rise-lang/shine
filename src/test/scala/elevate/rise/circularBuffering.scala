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
    iterateStream(fun(nbh =>
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

  private val id = fun(x => x)
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
        topDown(lowering.circularBuffer(id)) `;`
        topDown(lowering.iterateStream))
      -> circBuf,
    ))
  }

  def wrapExprChain(e: Rise): Rise = {
    infer(nFun(n => fun(
      ((n+6)`.`f32) ->: (n`.`f32)
    )(input =>
      e(input)
    )))
  }

  val highLevelChain: Rise =
    slide(4)(1) >> map(sum) >>
    slide(3)(1) >> map(sum) >>
    slide(2)(1) >> map(sum)

  val inlinedChain: Rise =
    slide(4)(1) >> map(sumSeq) >>
    slide(3)(1) >> map(sumSeq) >>
    slide(2)(1) >> mapSeq(sumSeq)

  val circBufChain: Rise =
    slide(4)(1) >> map(sumSeq) >>
    circularBuffer(3)(3)(id) >> mapStream(sumSeq) >>
    circularBuffer(2)(2)(id) >> iterateStream(sumSeq)

  test("example chain outputs are consistent") {
    val inlinedP = gen.CProgram(wrapExprChain(inlinedChain), "inlined")
    val circBufP = gen.CProgram(wrapExprChain(circBufChain), "circularBuffered")

    val N = 20
    val testCode =
      s"""
         |#include <stdio.h>
         |
         |${inlinedP.code}
         |${circBufP.code}
         |
         |int main(int argc, char** argv) {
         |  float input[$N+6];
         |
         |  for (int i = 0; i < $N+6; i++) {
         |    input[i] = (2 * i + 133) % 19;
         |  }
         |
         |  float output1[$N];
         |  inlined(output1, $N, input);
         |
         |  float output2[$N];
         |  circularBuffered(output2, $N, input);
         |
         |  for (int i = 0; i < $N; i++) {
         |    if (output1[i] != output2[i]) {
         |      fprintf(stderr, "(%f, %f)\\n",
         |        output1[i], output2[i]);
         |      return 1;
         |    }
         |  }
         |
         |  return 0;
         |}
         |""".stripMargin
    util.Execute(testCode)
  }

  def wrapExprTogether(e: Rise): Rise = {
    infer(nFun(n => nFun(m => fun(
      ((n+2)`.`(m+2)`.`f32) ->: (n`.`m`.`f32)
    )(input =>
      e(input)
    ))))
  }

  val highLevelTogether: Rise =
    fun(x => zip( // N+2.M+2.f
      x |> map(slide(3)(1) >> map(sum)), // N+2.M.f
      x |> map(slide(3)(1) >> map(sum)))) >>
    map(fun(p => zip(fst(p), snd(p)))) >> // N+2.M.(f x f)
    slide(3)(1) >> // N.3.M.(f x f)
    map(transpose >> // M.3.(f x f)
      map(map(fun(p => fst(p) + snd(p)))) >> // M.3.f
      map(sum) // M.f
    ) // N.M.f

  val inlinedTogether: Rise =
    fun(x => zip(
      x |> map(slide(3)(1) >> map(sumSeq)),
      x |> map(slide(3)(1) >> map(sumSeq)))) >>
      map(fun(p => zip(fst(p), snd(p)))) >>
      slide(3)(1) >>
      mapSeq(transpose >>
        map(map(fun(p => fst(p) + snd(p)))) >>
        mapSeq(sumSeq)
      )

  val circBufTogether: Rise =
    circularBuffer(3)(3)(
      slide(3)(1) >> mapSeq(fun(x => pair(sumSeq(x), sumSeq(x)))) >> unzip
    ) >> // N.3.(M.f x M.f)
    iterateStream(
      map(fun(p => zip(fst(p), snd(p)))) >> // 3.M.(f x f)
      transpose >>
      map(map(fun(p => fst(p) + snd(p)))) >>
      mapSeq(sumSeq)
    )

  test("example together outputs are consistent") {
    val inlinedP = gen.CProgram(wrapExprTogether(inlinedTogether), "inlined")
    val circBufP = gen.CProgram(wrapExprTogether(circBufTogether), "circularBuffered")

    val N = 8
    val M = 12
    val testCode =
      s"""
         |#include <stdio.h>
         |
         |${inlinedP.code}
         |${circBufP.code}
         |
         |int main(int argc, char** argv) {
         |  float input[$N+2 * $M+2];
         |
         |  for (int i = 0; i < $N+2 * $M+2; i++) {
         |    input[i] = (2 * i + 133) % 19;
         |  }
         |
         |  float output1[$N * $M];
         |  inlined(output1, $N, $M, input);
         |
         |  float output2[$N * $M];
         |  circularBuffered(output2, $N, $M, input);
         |
         |  for (int i = 0; i < $N * $M; i++) {
         |    if (output1[i] != output2[i]) {
         |      fprintf(stderr, "(%f, %f)\\n",
         |        output1[i], output2[i]);
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
