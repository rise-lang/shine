package apps

import lift.core._
import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import lift.OpenCL.DSL._
import lift.core.HighLevelConstructs._
import lift.core.semantics.FloatData

object binomialFilter {
  // Binomial filter, convolution is separable:
  //
  // 1 2 1   1
  // 2 4 2 ~ 2 x 1 2 1
  // 1 2 1   1

  val id: Expr = fun(x => x)
  val mulT: Expr = fun(x => fst(x) * snd(x))
  val dot: Expr = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduce(add)(l(0.0f))
  ))
  def dotSeq: Expr = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduceSeq(add)(l(0.0f))
  ))
  val dotSeqUnroll: Expr = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduceSeqUnroll(add)(l(0.0f))
  ))
  val dotSeqVecUnroll: Expr = fun(a => fun(b =>
    // TODO: Private
    zip(a)(b) |> map(mulT) |> oclReduceSeqUnroll(AddressSpace.Global)(add)(vectorFromScalar(l(0.0f)))
  ))

  val weights2d: Expr = larr(Seq(1, 2, 1, 2, 4, 2, 1, 2, 1).map(f => FloatData(f / 16.0f)))
  val weights1d: Expr = larr(Seq(1, 2, 1).map(f => FloatData(f / 4.0f)))

  private val slide3x3 = slide2D(3, 1)

  val base: Expr =
    padClamp2D(1) >> slide3x3 >> map(map(fun(nbh => dot(weights2d)(join(nbh)))))
  val baseSeq: Expr =
    padClamp2D(1) >> slide3x3 >> mapSeq(mapSeq(fun(nbh => dotSeq(weights2d)(join(nbh)))))

  val factorised: Expr =
    padClamp2D(1) >> slide3x3 >> map(map(map(dot(weights1d)) >> dot(weights1d)))
  val factorisedSeq: Expr =
    padClamp2D(1) >> slide3x3 >> mapSeq(mapSeq(map(dotSeq(weights1d)) >> dotSeq(weights1d)))

  val separated: Expr = {
    val horizontal = map(slide(3)(1) >> map(dot(weights1d)))
    val vertical = slide(3)(1) >> map(transpose >> map(dot(weights1d)))
    padClamp2D(1) >> vertical >> horizontal
  }
  val separatedSeq: Expr = {
    val horizontal = mapSeq(slide(3)(1) >> mapSeq(dotSeq(weights1d)))
    val vertical = slide(3)(1) >> mapSeq(transpose >> mapSeq(dotSeq(weights1d)))
    padClamp2D(1) >> vertical >> horizontal
  }

  val scanline: Expr =
    padClamp2D(1) >> slide(3)(1) >> map(transpose >>
      map(dot(weights1d)) >>
      slide(3)(1) >>
      map(dot(weights1d))
    )
  val scanlineSeq: Expr =
    padClamp2D(1) >> slide(3)(1) >> mapSeq(transpose >>
      mapSeq(dotSeq(weights1d)) >>
      slide(3)(1) >>
      mapSeq(dotSeq(weights1d))
    )

  val regRotSeq: Expr =
    padClamp2D(1) >> slide(3)(1) >> mapSeq(transpose >>
      map(dotSeq(weights1d)) >>
      slideSeq(SlideSeq.Values)(3)(1)(id)(dotSeq(weights1d))
    )
  val regRotPar: Expr = {
    val Dh = dotSeqVecUnroll(map(vectorFromScalar)(weights1d))
    val Dv = Dh
    val shuffle =
      asScalar >> drop(3) >> take(6) >> slide(4)(1) >> join >> asVector(4)
    map(padClamp(4)(4) >> asVector(4)) >> padClamp(1)(1) >>
    slide(3)(1) >> mapGlobal(transpose >>
      map(Dh) >>
      // TODO: Private
      oclSlideSeq(SlideSeq.Values)(AddressSpace.Global)(3)(1)(id)(shuffle >> Dv)
    )
  }
}
