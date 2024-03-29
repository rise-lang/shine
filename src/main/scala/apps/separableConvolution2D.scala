package apps

import rise.core._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.DSL.Type._
import rise.core.types._
import rise.core.types.DataType._
import rise.core.semantics._
import rise.openCL.DSL._
import rise.openCL.primitives.{oclRotateValues, oclReduceSeqUnroll}
import HighLevelConstructs._

object separableConvolution2D {
  def weights2d(scale: Float, ws: Seq[Seq[Int]]): ToBeTyped[Expr] =
    larr(ws.map(r => ArrayData(r.map(x => FloatData(x * scale)))))
  def weights1d(scale: Float, ws: Seq[Int]): ToBeTyped[Expr] =
    larr(ws.map(x => FloatData(x * scale)))

  // Binomial filter, convolution is separable:
  //
  // 1 2 1   1
  // 2 4 2 ~ 2 x 1 2 1
  // 1 2 1   1
  val binomialWeights2d: ToBeTyped[Expr] = weights2d(1.0f / 16.0f, Seq(
    Seq(1, 2, 1),
    Seq(2, 4, 2),
    Seq(1, 2, 1)
  ))
  val binomialWeightsV: ToBeTyped[Expr] = weights1d(1.0f / 4.0f, Seq(
    1, 2, 1
  ))
  val binomialWeightsH: ToBeTyped[Expr] = binomialWeightsV

  // Sobel filters, convolutions are separable:
  //
  // Sx:
  // -1  0 +1     1
  // -2  0 +2  ~  2 x -1  0 +1
  // -1  0 +1     1
  val sobelXWeights2d: ToBeTyped[Expr] = weights2d(1.0f / 8.0f, Seq(
    Seq(-1, 0, +1),
    Seq(-2, 0, +2),
    Seq(-1, 0, +1)
  ))
  val sobelXWeightsV: ToBeTyped[Expr] = weights1d(1.0f / 4.0f, Seq(
    1, 2, 1
  ))
  val sobelXWeightsH: ToBeTyped[Expr] = weights1d(1.0f / 2.0f, Seq(
    -1, 0, +1
  ))
  // Sy:
  // -1 -2 -1     -1
  //  0  0  0  ~   0 x 1 2 1
  // +1 +2 +1     +1
  val sobelYWeights2d: ToBeTyped[Expr] = weights2d(1.0f / 8.0f, Seq(
    Seq(-1, -2, -1),
    Seq( 0,  0,  0),
    Seq( 1,  2,  1)
  ))
  val sobelYWeightsV: ToBeTyped[Expr] = sobelXWeightsH
  val sobelYWeightsH: ToBeTyped[Expr] = sobelXWeightsV

  // Separable convolution expressions

  val id: ToBeTyped[Expr] = fun(x => x)
  val mulT: ToBeTyped[Expr] = fun(x => fst(x) * snd(x))
  val dot: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduce(add)(lf32(0.0f))
  ))
  val dotSeq: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduceSeq(add)(lf32(0.0f))
  ))
  val dotSeqUnroll: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduceSeqUnroll(add)(lf32(0.0f))
  ))
  val weightsSeqVecUnroll: ToBeTyped[Expr] = fun(weights => fun(vectors =>
    zip(map(vectorFromScalar)(weights))(vectors) |>
    map(mulT) |>
    oclReduceSeqUnroll(AddressSpace.Private)(add)(vectorFromScalar(lf32(0.0f)))
  ))

  val base: ToBeTyped[Expr] = fun(3`.`3`.`f32)(weights2d =>
    padClamp2D(1) >> slide2D(3, 1) >>
      map(map(fun(nbh => dot(join(weights2d))(join(nbh)))))
  )
  val baseSeq: ToBeTyped[Expr] = fun(3`.`3`.`f32)(weights2d =>
    padClamp2D(1) >> slide2D(3, 1) >>
      mapSeq(mapSeq(fun(nbh => dotSeqUnroll(join(weights2d))(join(nbh)))))
  )
  val baseVecU: ToBeTyped[Expr] = fun(3`.`3`.`f32)(weights2d =>
    padClamp2D(1) >> map(slideVectors(4) >> slide(3)(4)) >>
    slide(3)(1) >> map(transpose) >>
    mapSeq(mapSeq(fun(nbh => weightsSeqVecUnroll(join(weights2d))(join(nbh)))))
  )

  val factorised: ToBeTyped[Expr] = fun(3`.`f32)(weightsV => fun(3`.`f32)(weightsH =>
    padClamp2D(1) >> slide2D(3, 1) >>
    map(map(map(dot(weightsH)) >> dot(weightsV)))
  ))
  val factorisedSeq: ToBeTyped[Expr] = fun(3`.`f32)(weightsV => fun(3`.`f32)(weightsH =>
    padClamp2D(1) >> slide2D(3, 1) >>
    mapSeq(mapSeq(map(dotSeqUnroll(weightsH)) >> dotSeqUnroll(weightsV)))
  ))

  val factorisedVH: ToBeTyped[Expr] = fun(3`.`f32)(weightsV => fun(3`.`f32)(weightsH =>
    padClamp2D(1) >> slide2D(3, 1) >>
    map(map(transpose >> map(dot(weightsV)) >> dot(weightsH)))
  ))

  val separated: ToBeTyped[Expr] = fun(3`.`f32)(weightsV => fun(3`.`f32)(weightsH => {
    val horizontal = map(slide(3)(1) >> map(dot(weightsH)))
    val vertical = slide(3)(1) >> map(transpose >> map(dot(weightsV)))
    padClamp2D(1) >> vertical >> horizontal
  }))
  val separatedSeq: ToBeTyped[Expr] = fun(3`.`f32)(weightsV => fun(3`.`f32)(weightsH => {
    val horizontal = mapSeq(slide(3)(1) >> mapSeq(dotSeqUnroll(weightsH)))
    val vertical = slide(3)(1) >> toMemFun(mapSeq(
      transpose >> mapSeq(dotSeqUnroll(weightsV))
    ))
    padClamp2D(1) >> vertical >> horizontal
  }))

  val scanline: ToBeTyped[Expr] = fun(3`.`f32)(weightsV => fun(3`.`f32)(weightsH =>
    padClamp2D(1) >> slide(3)(1) >> map(
      transpose >>
      map(dot(weightsV)) >>
      slide(3)(1) >>
      map(dot(weightsH)))
  ))
  val scanlineSeq: ToBeTyped[Expr] = fun(3`.`f32)(weightsV => fun(3`.`f32)(weightsH =>
    padClamp2D(1) >> slide(3)(1) >> mapSeq(
      transpose >>
      mapSeq(dotSeqUnroll(weightsV)) >>
      slide(3)(1) >>
      mapSeq(dotSeqUnroll(weightsH)))
  ))
  val shuffle: ToBeTyped[Expr] =
    asScalar >> drop(3) >> take(6) >> slideVectors(4)
  val scanlinePar: ToBeTyped[Expr] = fun(3`.`f32)(weightsV => fun(3`.`f32)(weightsH =>
    map(impl{ w: Nat => fun(w`.`f32)(x =>
      x |> asVectorAligned(4)
        |> padCst(1)(0)(vectorFromScalar(x `@` lidx(0, w)))
        |> padCst(0)(1)(vectorFromScalar(x `@` lidx(w - 1, w)))
    )}) >> padClamp(1)(1) >>
    slide(3)(1) >> mapGlobal(
      transpose >>
      toGlobalFun(mapSeq(weightsSeqVecUnroll(weightsV))) >>
      slide(3)(1) >>
      mapSeq(shuffle >> weightsSeqVecUnroll(weightsH)) >>
      asScalar
    )
  ))

  val regRotSeq: ToBeTyped[Expr] = fun(3`.`f32)(weightsV => fun(3`.`f32)(weightsH =>
    padClamp2D(1) >> slide(3)(1) >> mapSeq(
      transpose >>
      map(dotSeqUnroll(weightsV)) >>
      rotateValues(3)(id) >>
      iterateStream(dotSeqUnroll(weightsH))
    )
  ))
  val regRotPar: ToBeTyped[Expr] = fun(3`.`f32)(weightsV => fun(3`.`f32)(weightsH => {
    val Dv = weightsSeqVecUnroll(weightsV)
    val Dh = weightsSeqVecUnroll(weightsH)
    // map(padClamp(4)(4) >> asVectorAligned(4)) >> padClamp(1)(1) >>
    map(impl{ w: Nat => fun(w`.`f32)(x =>
      x |> asVectorAligned(4)
        |> padCst(1)(0)(vectorFromScalar(x `@` lidx(0, w)))
        |> padCst(0)(1)(vectorFromScalar(x `@` lidx(w - 1, w)))
    )}) >> padClamp(1)(1) >>
    slide(3)(1) >> mapGlobal(
      transpose >>
      map(Dv) >>
      oclRotateValues(AddressSpace.Private)(3)(id) >>
      iterateStream(shuffle >> Dh) >>
      asScalar
    )
  }))

  def computeGold(
    h: Int,
    w: Int,
    input: Array[Array[Float]],
    weights: Array[Array[Float]]
  ): Array[Array[Float]] = {
    val output = Array.fill(h, w)(Float.NaN)
    val lastY = h - 1
    val lastX = w - 1
    val ws = weights

    for (y <- input.indices) {
      val r0 = if (y > 0) input(y - 1) else input(0)
      val r1 = input(y)
      val r2 = if (y < lastY) input(y + 1) else input(lastY)
      for (x <- r1.indices) {
        val c0 = if (x > 0) x - 1 else 0
        val c1 = x
        val c2 = if (x < lastX) x + 1 else lastX
        output(y)(x) =
          ws(0)(0) * r0(c0) + ws(0)(1) * r0(c1) + ws(0)(2) * r0(c2) +
          ws(1)(0) * r1(c0) + ws(1)(1) * r1(c1) + ws(1)(2) * r1(c2) +
          ws(2)(0) * r2(c0) + ws(2)(1) * r2(c1) + ws(2)(2) * r2(c2)
      }
    }

    output
  }

  def computeGold(
    h: Int,
    w: Int,
    input: Array[Array[Float]],
    weights: ToBeTyped[Expr]
  ): Array[Array[Float]] = {
    import rise.core.semantics._
    weights match {
      case ToBeTyped(Literal(ArrayData(a))) =>
        computeGold(h, w, input,
          a.map(r =>
            r.asInstanceOf[ArrayData].a.map(x =>
              x.asInstanceOf[FloatData].f
            ).toArray
          ).toArray
        )
      case _ => ???
    }
  }
}
