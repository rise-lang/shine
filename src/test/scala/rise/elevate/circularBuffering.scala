package rise.elevate

import _root_.util.{Execute, gen}
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import rise.core.DSL.HighLevelConstructs.dropLast
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.makeClosed
import rise.core.primitives._
import rise.core.types.DataType._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.movement._
import rise.elevate.rules.traversal.alternative
import rise.elevate.rules.traversal.alternative._
import rise.elevate.rules.{lowering, _}
import rise.elevate.strategies.predicate._
import shine.DPIA.Nat

object circularBuffering {
  private val sum = reduce(add)(lf32(0.0f))
  private val sumSeq = reduceSeq(add)(lf32(0.0f))

  def wrapExpr(e: ToBeTyped[Rise]): ToBeTyped[Rise] = {
    depFun((n: Nat) => fun(
      ((n+5)`.`f32) ->: (2`.`n`.`f32)
    )(input =>
      e(input)
    ))
  }

  val highLevel: ToBeTyped[Rise] =
    slide(3)(1) >> map(sum) >> fun(x =>
      makeArray(2)(
        x |> slide(2)(1) >> map(sum) >> drop(1) >> dropLast(1))(
        x |> slide(4)(1) >> map(sum)
      ))

  val inlined: ToBeTyped[Rise] =
    slide(3)(1) >> map(sumSeq) >> fun(x =>
      makeArray(2)(
        x |> slide(2)(1) >> map(sumSeq) >> drop(1) >> dropLast(1))(
        x |> slide(4)(1) >> map(sumSeq)
      )) >> mapSeqUnroll(mapSeq(fun(x => x)))

  val buffered: ToBeTyped[Rise] =
    slide(3)(1) >> mapSeq(sumSeq) >> toMem >> letf(x =>
      makeArray(2)(
        x |> slide(2)(1) >> map(sumSeq) >> drop(1) >> dropLast(1))(
        x |> slide(4)(1) >> map(sumSeq)
      )) >> mapSeqUnroll(mapSeq(fun(x => x)))

  val circBuf: ToBeTyped[Rise] =
    slide(3)(1) >> map(sumSeq) >>
    circularBuffer(4)(4)(fun(x => x)) >>
    iterateStream(fun(nbh =>
      makeArray(2)(
        nbh |> drop(1) >> dropLast(1) >> sumSeq)(
        nbh |> sumSeq
      ) |> mapSeqUnroll(fun(x => x))
    )) >> transpose

  def wrapExprChain(e: ToBeTyped[Rise]): ToBeTyped[Rise] = {
    depFun((n: Nat) => fun(
      ((n+6)`.`f32) ->: (n`.`f32)
    )(input =>
      e(input)
    ))
  }

  val highLevelChain: ToBeTyped[Rise] =
    slide(4)(1) >> map(sum) >>
    slide(3)(1) >> map(sum) >>
    slide(2)(1) >> map(sum)

  val inlinedChain: ToBeTyped[Rise] =
    slide(4)(1) >> map(sumSeq) >>
    slide(3)(1) >> map(sumSeq) >>
    slide(2)(1) >> mapSeq(sumSeq)

  val circBufChain: ToBeTyped[Rise] =
    slide(4)(1) >> // map(sumSeq) >>
    circularBuffer(3)(3)(sumSeq) >> // mapStream(sumSeq) >>
    circularBuffer(2)(2)(sumSeq) >> iterateStream(sumSeq)

  def wrapExprTogether(e: ToBeTyped[Rise]): ToBeTyped[Rise] = {
    depFun((n: Nat, m: Nat) => fun(
      ((n+2)`.`(m+2)`.`f32) ->: (n`.`m`.`f32)
    )(input =>
      e(input)
    ))
  }

  val highLevelTogether: ToBeTyped[Rise] =
    fun(x => zip( // N+2.M+2.f
    x |> map(slide(3)(1) >> map(sum)))( // N+2.M.f
    x |> map(slide(3)(1) >> map(sum)))) >>
    map(fun(p => zip(fst(p))(snd(p)))) >> // N+2.M.(f x f)
    slide(3)(1) >> // N.3.M.(f x f)
    map(transpose >> // M.3.(f x f)
      map(map(fun(p => fst(p) + snd(p)))) >> // M.3.f
      map(sum) // M.f
    ) // N.M.f

  val inlinedTogether: ToBeTyped[Rise] =
    fun(x => zip(
    x |> map(slide(3)(1) >> map(sumSeq)))(
    x |> map(slide(3)(1) >> map(sumSeq)))) >>
    map(fun(p => zip(fst(p))(snd(p)))) >>
    slide(3)(1) >>
    mapSeq(transpose >>
      map(map(fun(p => fst(p) + snd(p)))) >>
      mapSeq(sumSeq)
    )

  val circBufTogether: ToBeTyped[Rise] =
    circularBuffer(3)(3)(
      slide(3)(1) >> mapSeq(fun(x => makePair(sumSeq(x))(sumSeq(x)))) >> unzip
    ) >> // N.3.(M.f x M.f)
    iterateStream(
      map(fun(p => zip(fst(p))(snd(p)))) >> // 3.M.(f x f)
        transpose >>
        map(map(fun(p => fst(p) + snd(p)))) >>
        mapSeq(sumSeq)
    )
}

class circularBuffering extends test_util.Tests {
  import circularBuffering._

  test("example outputs are consistent") {
    val inlinedFun = gen.c.function("inlined").asStringFromExpr(wrapExpr(inlined))
    val bufferedFun = gen.c.function("buffered").asStringFromExpr(wrapExpr(buffered))
    val circBufFun = gen.c.function("circularBuffered").asStringFromExpr(wrapExpr(circBuf))

    val N = 20
    val testCode =
      s"""
         |#include <stdio.h>
         |
         |$inlinedFun
         |$bufferedFun
         |$circBufFun
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
    Execute(testCode)
  }

  private def openEquality(typedA: Rise, b: Rise): Boolean = {
    import rise.core.DSL._
    val typedB: Rise = toBeTyped(b) !: typedA.t
    makeClosed(typedA) =~= makeClosed(typedB)
  }

  private val id = fun(x => x)
  private val norm = normalize[rise.core.Expr](gentleBetaReduction())(using alternative.RiseTraversable) // TODO: check if swapping of argument order is what we want!!!

  private def rewriteSteps(a: Rise, steps: scala.collection.Seq[(Strategy[Rise], Rise)]): Unit = {
    steps.foldLeft[Rise](norm(a).get)({ case (e, (s, expected)) =>
      val result = (s `;` norm)(e).get
      val nuExpect = norm(expected).get
      if (!openEquality(result, nuExpect)) {
        throw new Exception(
          s"expected structural equality.\nGot:\n$result\nExpected:\n$nuExpect")
      }
      result
    })
  }

  test("highLevel to circBuf") {
    rewriteSteps(highLevel, scala.collection.Seq(
      (topDown(dropBeforeMap)`;` topDown(takeBeforeMap))
      -> (
        slide(3)(1) >> map(sum) >> fun(x =>
        makeArray(2)(
          x |> slide(2)(1) >> drop(1) >> dropLast(1) >> map(sum))(
          x |> slide(4)(1) >> map(sum)
        ))
      ),
      (topDown(dropInSlide) `;` topDown(takeBeforeMap) `;` topDown(takeInSlide))
      -> (
      slide(3)(1) >> map(sum) >> fun(x =>
        makeArray(2)(
          x |> slide(4)(1) >> map(dropLast(1)) >> map(drop(1)) >> map(sum))(
          x |> slide(4)(1) >> map(sum)
        ))
      ),
      normalize(mapFusion)
      -> (
      slide(3)(1) >> map(sum) >> fun(x =>
        makeArray(2)(
          x |> slide(4)(1) >> map(dropLast(1) >> drop(1) >> sum))(
          x |> slide(4)(1) >> map(sum)
        ))
      ),
      topDown(mapOutsideMakeArray)
      -> (
      slide(3)(1) >> map(sum) >> slide(4)(1) >> map(fun(x =>
        makeArray(2)(
          x |> dropLast(1) >> drop(1) >> sum)(
          x |> sum
        ))) >> transpose
      ),
      (normalize(lowering.reduceSeq) `;`
        topDown(dropBeforeTake) `;`
        topDown(isApply `;` one(isApply `;` one(isMakeArray)) `;`
          lowering.mapSeqUnrollWrite) `;`
        topDown(lowering.circularBuffer(id)) `;`
        topDown(lowering.iterateStream))
      -> circBuf,
    ))
  }

  test("example chain outputs are consistent") {
    val inlinedFun = gen.c.function("inlined").asStringFromExpr(wrapExprChain(inlinedChain))
    val circBufFun = gen.c.function("circularBuffered").asStringFromExpr(wrapExprChain(circBufChain))

    val N = 20
    val testCode =
      s"""
         |#include <stdio.h>
         |
         |$inlinedFun
         |$circBufFun
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
    Execute(testCode)
  }

  test("example together outputs are consistent") {
    val inlinedFun = gen.c.function("inlined").asStringFromExpr(wrapExprTogether(inlinedTogether))
    val circBufFun = gen.c.function("circularBuffered").asStringFromExpr(wrapExprTogether(circBufTogether))

    val N = 8
    val M = 12
    val testCode =
      s"""
         |#include <stdio.h>
         |
         |$inlinedFun
         |$circBufFun
         |
         |int main(int argc, char** argv) {
         |  float input[${N+2} * ${M+2}];
         |
         |  for (int i = 0; i < ${N+2} * ${M+2}; i++) {
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
    Execute(testCode)
  }
}
