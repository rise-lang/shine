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

    gen.CProgram(wrapExpr(inlined), "inlined")
    gen.CProgram(wrapExpr(buffered), "buffered")
    gen.CProgram(wrapExpr(circBuf), "circularBuffered")

    // TODO: check outputs
  }
}
