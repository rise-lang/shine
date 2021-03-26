package apps

import apps.separableConvolution2D._
import rise.core.primitives._
import rise.core.DSL._
import rise.core.DSL.HighLevelConstructs._
import rise.core.DSL.Type._
import rise.openCL.primitives.oclReduceSeqUnroll
import rise.openCL.DSL._
import rise.core._
import rise.core.types._
import shine.OpenCL.KernelExecutor.KernelNoSizes.fromKernelModule
import util.gen

class convolution1D extends test_util.Tests {
  val binomialWeights = binomialWeightsV

  val dotSeq: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |>
    oclReduceSeqUnroll(AddressSpace.Private)(add)(lf32(0.0f))
  ))

  val binomial: ToBeTyped[Expr] =
    slide(3)(1) >> map(fun(nbh => dot(nbh)(binomialWeights)))

  val binomialSeq: ToBeTyped[Expr] =
    slide(3)(1) >> mapSeq(fun(nbh => dotSeq(nbh)(binomialWeights)))

  val binomialTile: ToBeTyped[Expr] =
    slide(34)(32) >>
    mapGlobal(0)(
      slide(3)(1) >> mapSeq(fun(nbh => dotSeq(nbh)(binomialWeights)))
    ) >> join

  val binomialTileShiftInwardsGP: ToBeTyped[Expr] =
    tileShiftInwards(32)(mapGlobal(0)(
      slide(3)(1) >> mapSeq(fun(nbh => dotSeq(nbh)(binomialWeights)))
    ))

  val binomialTileShiftInwardsWLP: ToBeTyped[Expr] =
    tileShiftInwards(32)(mapWorkGroup(0)(
      slide(3)(1) >> mapLocal(0)(fun(nbh => dotSeq(nbh)(binomialWeights)))
    ))

  val binomialTileEpilogue: ToBeTyped[Expr] = {
    def f = mapGlobal(0)(
      slide(3)(1) >> mapSeq(fun(nbh => dotSeq(nbh)(binomialWeights)))
    )
    tileEpilogue(32)(f)(f)
  }

  val binomialTileDep: ToBeTyped[Expr] = impl{ n: Nat =>
    // depSlide(34)(32) >>
    depTile(32)(
      depMapSeq(depFun { i: Nat => // TODO: depMapGlobal(0)
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

  private def wrapExpr(e: ToBeTyped[Expr]): ToBeTyped[Expr] = {
    import arithexpr.arithmetic.{PosInf, RangeAdd}
    // at least 3*4 = 12 for one vector sliding window
    depFun(RangeAdd(12, PosInf, 4), (n: Nat) => fun(((n+2)`.`f32) ->: (n`.`f32))(a => e(a)))
  }

  import shine.OpenCL.{GlobalSize, LocalSize}

  private def checkOCL(
    N: Int,
    localSize: LocalSize,
    globalSize: GlobalSize,
    e: ToBeTyped[Expr]
  ): Unit = {
    import shine.OpenCL._

    val random = new scala.util.Random()
    val input = Array.fill(N+2)(random.nextFloat())

    val goldKernel = gen.opencl.kernel.fromExpr(wrapExpr(binomialSeq))
    val kernel = gen.opencl.kernel.fromExpr(wrapExpr(e))

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

  test("binomialTileShiftInwardsGP compiles to valid OpenCL that passes checks") {
    util.withExecutor {
      checkOCL(128, LocalSize(1), GlobalSize(64), binomialTileShiftInwardsGP)
      checkOCL(132, LocalSize(1), GlobalSize(64), binomialTileShiftInwardsGP)
      checkOCL(148, LocalSize(1), GlobalSize(64), binomialTileShiftInwardsGP)
    }
  }

  test("binomialTileShiftInwardsWLP compiles to valid OpenCL that passes checks") {
    util.withExecutor {
      checkOCL(128, LocalSize(16), GlobalSize(128), binomialTileShiftInwardsWLP)
      checkOCL(132, LocalSize(32), GlobalSize(128), binomialTileShiftInwardsWLP)
      checkOCL(148, LocalSize(64), GlobalSize(128), binomialTileShiftInwardsWLP)
    }
  }

  // TODO: nat normal form + concat codegen
  ignore("binomialTileEpilogue compiles to valid OpenCL that passes checks") {
    util.withExecutor {
      checkOCL(128, LocalSize(1), GlobalSize(2), binomialTileEpilogue)
      checkOCL(132, LocalSize(1), GlobalSize(2), binomialTileEpilogue)
      checkOCL(148, LocalSize(1), GlobalSize(2), binomialTileEpilogue)
    }
  }

  // TODO: codegen + parallelism
  ignore("binomialTileDep compiles to valid OpenCL that passes checks") {
    util.withExecutor {
      checkOCL(128, LocalSize(1), GlobalSize(1), binomialTileDep)
      checkOCL(132, LocalSize(1), GlobalSize(1), binomialTileDep)
      checkOCL(148, LocalSize(1), GlobalSize(1), binomialTileDep)
    }
  }

  // TODO: nat unification
  ignore("depSlide inference") {
    println(
      depFun((n: Nat) => fun(((n+2)`.`f32) ->: (n`.`f32))(a =>
        a |>
        depSlide(n+2)(34)(32) >>
        depMapSeq(depFun { i: Nat =>
          import arithexpr.arithmetic.IfThenElse
          import arithexpr.arithmetic.BoolExpr.arithPredicate
          import arithexpr.arithmetic.BoolExpr.ArithPredicate.Operator

          val fullWindows = n / 32
          val remainder = n % 32
          val m: Nat = IfThenElse(arithPredicate(i, fullWindows, Operator.<), 32, remainder)
          (slide(3)(1) >> mapSeq(fun(nbh => dotSeq(nbh)(binomialWeights)))
            ) :: ((m + 2) `.` f32) ->: (m `.` f32)
        }) >> depJoin
      )).toExpr.t
    )
  }
}
