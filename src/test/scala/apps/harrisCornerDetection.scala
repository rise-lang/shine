package apps

import lift.core._
import lift.core.primitives._
import lift.core.semantics._
import lift.core.DSL._
import lift.core.types._
import lift.core.HighLevelConstructs._

import idealised.util.gen

object harrisCornerDetection {
  val mulT = binomialFilter.mulT
  val add = binomialFilter.add
  val sq = fun(x => x * x)
  val dot = binomialFilter.dot
  val dotSeq = binomialFilter.dotSeq

  // arbitrary values
  val kappa = l(1.0f)
  val threshold = l(1.0f)

  val sobelXA = Array(Array(-1, 0, 1), Array(2, 0, 2), Array(-1, 0, 1))
      .map(a => a.map(v => FloatData(v / 8.0f)))

  val sobelX = larr(sobelXA.flatten : Array[Data])
  val sobelY = larr(sobelXA.transpose.flatten : Array[Data])

  val slide3x3 = map(slide(3)(1)) >> slide(3)(1) >> map(transpose)

  def stencil3x3(weights: Expr): Expr =
    slide3x3 >> mapSeq(mapSeq(fun(nbh => dotSeq(weights)(join(nbh)))))

  val zip2D = zipND(2)

  val gaussian: Expr = binomialFilter.regrot

  val szr = lift.arithmetic.RangeAdd(3, lift.arithmetic.PosInf, 1)
  // TODO: store temporaries
  val e = nFun(szr, h => nFun(szr, w => fun(ArrayType(h, ArrayType(w, float)))(i => {
    val ix = stencil3x3(sobelX)(i)
    val iy = stencil3x3(sobelY)(i)

    val ixx = mapSeq(mapSeq(sq))(ix)
    val iyy = mapSeq(mapSeq(sq))(iy)
    val ixy = zip2D(ix)(iy) |> mapSeq(mapSeq(mulT))

    val sxx = gaussian(ixx)
    val sxy = gaussian(ixy)
    val syy = gaussian(iyy)

    val coarsity = zip2D(sxx)(zip2D(syy)(sxy)) |> mapSeq(mapSeq(fun(s => {
      val sxx = fst(s)
      val syy = fst(snd(s))
      val sxy = snd(snd(s))

      val det = sxx * syy - sxy * sxy
      val trace = sxx + syy
      det - kappa * trace * trace
    })))

    /* TODO
    val threshold = coarsity :>> mapSeq(mapSeq(fun(c =>
      if (c <= threshold) { 0.0f } else { c }
    )))

    val edge = slide3x3(coarsity) :>> mapSeq(mapSeq(fun(nbh =>
      forall i != center, nbh[center] > nbh[i] ?
    )))
    */

    coarsity
  })))
}

class harrisCornerDetection extends idealised.util.Tests {
  test("harris compiles to C code") {
    gen.CProgram(harrisCornerDetection.e, "harris")
  }
}
