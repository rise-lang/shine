package idealised.apps

import idealised._
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.Semantics._
import idealised.util.{Execute, SyntaxChecker}

object harrisCornerDetection {
  val mul2 = fun(t => t._1 * t._2)
  val add = fun(x => fun(a => x + a))
  val sq = fun(x => x * x)
  val dot = fun(a => fun(b => zip(a, b) :>> map(mul2) :>> reduceSeq(add, 0.0f)))

  // arbitrary values
  val kappa = LiteralExpr(FloatData(1.0f))
  val threshold = LiteralExpr(FloatData(1.0f))

  val sobelXA = Array(Array(-1, 0, 1), Array(2, 0, 2), Array(-1, 0, 1))
      .map(a => a.map(v => FloatData(v / 8.0f)))

  val sobelX = LiteralExpr(ArrayData(sobelXA.flatten: Array[Data]))
  val sobelY = LiteralExpr(ArrayData(sobelXA.transpose.flatten: Array[Data]))

  val slide3x3 = map(slide(3, 1)) >>> slide(3, 1) >>> map(transpose())

  def stencil3x3(weights: Expr): Expr =
    slide3x3 >>> mapSeq(mapSeq(fun(nbh => dot(weights)(join(nbh)))))

  val zip2d = fun(a => fun(b =>
    zip(a, b) :>> map(fun(al_bl => zip(al_bl._1, al_bl._2)))))

  val gaussian = idealised.apps.binomialFilter.blur

  // TODO: store temporaries
  val e = nFun(h => nFun(w => fun(ArrayType(h, ArrayType(w, float)))(i => {
    val ix = stencil3x3(sobelX)(i)
    val iy = stencil3x3(sobelY)(i)

    val ixx = mapSeq(mapSeq(sq), ix)
    val iyy = mapSeq(mapSeq(sq), iy)
    val ixy = zip2d(ix)(iy) :>> mapSeq(mapSeq(mul2))

    val sxx = gaussian(h)(w)(ixx)
    val sxy = gaussian(h)(w)(ixy)
    val syy = gaussian(h)(w)(iyy)

    val coarsity = zip2d(sxx)(zip2d(syy)(sxy)) :>> mapSeq(mapSeq(fun(s => {
      val sxx = s._1
      val syy = s._2._1
      val sxy = s._2._2

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
  def program(name: String, e: Expr): C.Program = {
    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(e, collection.Map()))
    val program = C.ProgramGenerator.makeCode(phrase, name)
    SyntaxChecker(program.code)
    println(program.code)
    program
  }

  test("harris compiles to C code") {
    program("harris", harrisCornerDetection.e)
  }
}
