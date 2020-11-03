package apps

import apps.separableConvolution2D._
import rise.core.DSL._
import rise.openCL.DSL._
import rise.core.HighLevelConstructs._
import rise.core.TypeLevelDSL._
import rise.core._
import rise.core.types._
import util.gen

class convolution1D extends test_util.Tests {
  val binomialWeights = binomialWeightsV

  val dotSeq: Expr = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |>
    oclReduceSeqUnroll(AddressSpace.Private)(add)(l(0.0f))
  ))

  val binomial: Expr =
    slide(3)(1) >> map(fun(nbh => dot(nbh)(binomialWeights)))

  val binomialSeq: Expr =
    slide(3)(1) >> mapSeq(fun(nbh => dotSeq(nbh)(binomialWeights)))

  val binomialTile: Expr =
    slide(34)(32) >>
    mapGlobal(0)(
      slide(3)(1) >> mapSeq(fun(nbh => dotSeq(nbh)(binomialWeights)))
    ) >> join

  // FIXME: running in parallel will trigger race condition?
  val binomialTileShiftInwards: Expr =
    tileShiftInwards(32)(mapGlobal(0)(
      slide(3)(1) >> mapSeq(fun(nbh => dotSeq(nbh)(binomialWeights)))
    ))
    /*tileShiftInwards(32)(mapWorkGroup(0)(
      slide(3)(1) >> mapLocal(0)(fun(nbh => dotSeq(nbh)(binomialWeights)))
    ))*/

  val binomialTileEpilogue: Expr = {
    def f = mapGlobal(0)(
      slide(3)(1) >> mapSeq(fun(nbh => dotSeq(nbh)(binomialWeights)))
    )
    tileEpilogue(32)(f)(f)
  }

  val binomialTileDep: Expr = impl{ n: Nat =>
    // depSlide(34)(32) >>
    depTile(32)(
      depMapSeq(nFun { i => // TODO: depMapGlobal(0)
        import arithexpr.arithmetic.IfThenElse
        import arithexpr.arithmetic.BoolExpr.arithPredicate
        import arithexpr.arithmetic.BoolExpr.ArithPredicate.Operator

        val fullWindows = n / 32
        val remainder = n % 32
        val m: Nat = IfThenElse(arithPredicate(i, fullWindows, Operator.<), 32, remainder)
        (slide(3)(1) >> mapSeq(fun(nbh => dotSeq(nbh)(binomialWeights)))
          ) :: ((m+2) `.` f32) ->: (m `.` f32)
      })
    )
  }

  private def wrapExpr(e: Expr): Expr = {
    import arithexpr.arithmetic.{PosInf, RangeAdd}
    // at least 3*4 = 12 for one vector sliding window
    nFun(RangeAdd(12, PosInf, 4), n => fun(((n+2)`.`f32) ->: (n`.`f32))(a => e(a)))
  }

  import shine.OpenCL.{GlobalSize, LocalSize}

  private def checkOCL(
    N: Int,
    localSize: LocalSize,
    globalSize: GlobalSize,
    e: Expr
  ): Unit = {
    import shine.OpenCL._

    val random = new scala.util.Random()
    val input = Array.fill(N+2)(random.nextFloat())

    val goldKernel = gen.OpenCLKernel(wrapExpr(binomialSeq))
    val kernel = gen.OpenCLKernel(wrapExpr(e))

    val goldRun = goldKernel.as[ScalaFunction `(`
      Int `,` Array[Float]
      `)=>` Array[Float]]
    val (gold, _) = goldRun(LocalSize(1), GlobalSize(1))(N `,` input)

    val run = kernel.as[ScalaFunction `(`
      Int `,` Array[Float]
      `)=>` Array[Float]]
    val (output, time) = run(localSize, globalSize)(N `,` input)
    util.assertSame(output, gold, "output is different from gold")
    println(s"time: $time")
  }

  test("binomialTile compiles to valid OpenCL that passes checks") {
    util.withExecutor {
      checkOCL(128, LocalSize(1), GlobalSize(2), binomialTile)
      // expected to fail:
      // checkOCL(130, LocalSize(1), GlobalSize(2), binomialTile)
    }
  }

  test("binomialTileShiftInwards compiles to valid OpenCL that passes checks") {
    // FIXME: race condition?
    util.withExecutor {
      checkOCL(128, LocalSize(1), GlobalSize(64), binomialTileShiftInwards)
      checkOCL(132, LocalSize(1), GlobalSize(64), binomialTileShiftInwards)
      checkOCL(148, LocalSize(1), GlobalSize(64), binomialTileShiftInwards)
    }
  }

  // TODO: concat codegen
  ignore("binomialTileEpilogue compiles to valid OpenCL that passes checks") {
    util.withExecutor {
      checkOCL(128, LocalSize(1), GlobalSize(2), binomialTileEpilogue)
      checkOCL(132, LocalSize(1), GlobalSize(2), binomialTileEpilogue)
      checkOCL(148, LocalSize(1), GlobalSize(2), binomialTileEpilogue)
    }
  }

  // TODO: codegen+parallelism
  ignore("binomialTileDep compiles to valid OpenCL that passes checks") {
    util.withExecutor {
      checkOCL(128, LocalSize(1), GlobalSize(1), binomialTileDep)
      checkOCL(132, LocalSize(1), GlobalSize(1), binomialTileDep)
      checkOCL(148, LocalSize(1), GlobalSize(1), binomialTileDep)
    }
  }
}
