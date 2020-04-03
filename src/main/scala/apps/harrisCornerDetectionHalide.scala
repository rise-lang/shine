package apps

import rise.core._
import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types._
import rise.core.HighLevelConstructs._

object harrisCornerDetectionHalide {
  private val C2D = separableConvolution2D
  private val mulT = C2D.mulT
  private val dot = C2D.dot

  private def larr_f32(s: Seq[Float]): Expr = {
    larr(s.map(semantics.FloatData))
  }

  val gray: Expr = nFun(h => nFun(w => fun(
    (3`.`h`.`w`.`f32) ->: (h`.`w`.`f32)
  )(input => input |>
    transpose >> map(transpose) >>
    map(map(dot(larr_f32(Seq(0.299f, 0.587f, 0.114f)))))
  )))

  val sobelXWeights2d: Expr = C2D.weights2d(1.0f / 12.0f, Seq(
    Seq(-1, 0, +1),
    Seq(-2, 0, +2),
    Seq(-1, 0, +1)
  ))
  val sobelXWeightsV: Expr = C2D.weights1d(1.0f, Seq(
    1, 2, 1
  ))
  val sobelXWeightsH: Expr = C2D.weights1d(1.0f / 12.0f, Seq(
    -1, 0, +1
  ))

  val sobelYWeights2d: Expr = C2D.weights2d(1.0f / 12.0f, Seq(
    Seq(-1, -2, -1),
    Seq( 0,  0,  0),
    Seq( 1,  2,  1)
  ))
  val sobelYWeightsV: Expr = C2D.weights1d(1.0f, Seq(
    -1, 0, +1
  ))
  val sobelYWeightsH: Expr = C2D.weights1d(1.0f / 12.0f, Seq(
    1, 2, 1
  ))

  val conv3x3: Expr = fun(3`.`3`.`f32)(weights =>
    nFun(h => nFun(w => fun(
      ((h+2)`.`(w+2)`.`f32) ->: (h`.`w`.`f32)
    )(input => input |>
      slide2D(3, 1) >>
      map(map(fun(nbh => dot(join(weights))(join(nbh)))))
    )))
  )
  val sobelX: Expr = conv3x3(sobelXWeights2d)
  val sobelY: Expr = conv3x3(sobelYWeights2d)

  val mul = nFun(h => nFun(w => fun(
    (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32)
  )((a, b) =>
    zipND(2)(a, b) |> map(map(mulT))
  )))

  val sum: Expr = reduce(add)(l(0.0f))
  val sum3x3: Expr = nFun(h => nFun(w => fun(
    ((h+2)`.`(w+2)`.`f32) ->: (h`.`w`.`f32)
  )(input => input |>
    slide2D(3, 1) >>
    map(map(fun(nbh => sum(join(nbh)))))
  )))

  // halide: det + trace + output
  val coarsityElem = fun(sxx => fun(sxy => fun(syy => fun(kappa => {
    val det = sxx * syy - sxy * sxy
    val trace = sxx + syy
    det - kappa * trace * trace
  }))))
  val coarsity = nFun(h => nFun(w => fun(
    (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32)
  )((sxx, sxy, syy) =>
    zipND(2)(sxx, zipND(2)(sxy, syy)) |> map(map(fun { s =>
      val sxx = fst(s)
      val sxy = fst(snd(s))
      val syy = snd(snd(s))
      coarsityElem(sxx)(sxy)(syy)(l(0.04f))
    }))
  )))

  // note: the output is strided:
  // it only contains w-4 meaningful values per line
  // so 4 meaningless values at the end of each line
  // this way if the input is dividable by a vector width,
  // so is the output
  def harris(hMod: Int, wMod: Int): Expr =
    nModFun(hMod, h => nModFun(wMod, w => fun(
      // (3`.`(h+4)`.`(w+4)`.`f32) ->: (h`.`w`.`f32)
      (3`.`(h+4)`.`w`.`f32) ->: (h`.`w`.`f32)
    )(input => { input |>
      gray(h+4)(w) |> fun(g =>
      sobelX(h+2)(w-2)(g) |> fun(ix =>
      sobelY(h+2)(w-2)(g) |> fun(iy =>
      mul(h+2)(w-2)(ix)(ix) |> fun(ixx =>
      mul(h+2)(w-2)(ix)(iy) |> fun(ixy =>
      mul(h+2)(w-2)(iy)(iy) |> fun(iyy =>
      sum3x3(h)(w-4)(ixx) |> fun(sxx =>
      sum3x3(h)(w-4)(ixy) |> fun(sxy =>
      sum3x3(h)(w-4)(iyy) |> fun(syy =>
      coarsity(h)(w-4)(sxx)(sxy)(syy) |> map(padEmpty(4))
      )))))))))
    })))

  private val id = fun(x => x)
  private val write1DSeq = mapSeq(id)
  private val write2DSeq = mapSeq(write1DSeq)

  object gen {
    def harrisSeqWrite(letMem: Expr): Expr =
      nFun(h => nFun(w => fun(
       (3`.`(h+4)`.`w`.`f32) ->: (h`.`w`.`f32)
      )(input => input |>
        gray(h+4)(w) |> write2DSeq |> letMem(fun(g =>
        sobelX(h+2)(w-2)(g) |> write2DSeq |> letMem(fun(ix =>
        sobelY(h+2)(w-2)(g) |> write2DSeq |> letMem(fun(iy =>
        mul(h+2)(w-2)(ix)(ix) |> write2DSeq |> letMem(fun(ixx =>
        mul(h+2)(w-2)(ix)(iy) |> write2DSeq |> letMem(fun(ixy =>
        mul(h+2)(w-2)(iy)(iy) |> write2DSeq |> letMem(fun(iyy =>
        sum3x3(h)(w-4)(ixx) |> write2DSeq |> letMem(fun(sxx =>
        sum3x3(h)(w-4)(ixy) |> write2DSeq |> letMem(fun(sxy =>
        sum3x3(h)(w-4)(iyy) |> write2DSeq |> letMem(fun(syy =>
        coarsity(h)(w-4)(sxx)(sxy)(syy) |> write2DSeq |> map(padEmpty(4))
        ))))))))))))))))))
      )))

    def harrisBuffered(circularBuffer: Expr): Expr =
      nFun(h => nFun(w => fun(
        (3`.`(h+4)`.`w`.`f32) ->: (h`.`w`.`f32)
      )(input => input |>
        transpose >> map(transpose) >>
        map(map(dot(larr_f32(Seq(0.299f, 0.587f, 0.114f))))) >>
        circularBuffer(3)(3)(write1DSeq) >>
        circularBuffer(3)(3)(
          map(slide(3)(1)) >> transpose >>
          map(fun(nbh => pair(
            dot(join(sobelXWeights2d))(join(nbh)),
            dot(join(sobelYWeights2d))(join(nbh))
          ))) >> write1DSeq >> unzip
        ) >> // H.3.(W.f x W.f)
        iterateStream(
          map(fun(p => zip(fst(p), snd(p)))) >> fun(ixiy => // 3.W.(f x f)
          ixiy |> map(map(fun(p => fst(p) * fst(p)))) |> fun(ixx =>
          ixiy |> map(map(fun(p => fst(p) * snd(p)))) |> fun(ixy =>
          ixiy |> map(map(fun(p => snd(p) * snd(p)))) |> fun(iyy =>
          slide2D(3, 1)(ixx) |> map(map(fun(nbh => sum(join(nbh))))) |>
          fun(sxx =>
          slide2D(3, 1)(ixy) |> map(map(fun(nbh => sum(join(nbh))))) |>
          fun(sxy =>
          slide2D(3, 1)(iyy) |> map(map(fun(nbh => sum(join(nbh))))) |>
          fun(syy =>
            zipND(2)(sxx, zipND(2)(sxy, syy)) |> map(map(fun { s =>
              val sxx = fst(s)
              val sxy = fst(snd(s))
              val syy = snd(snd(s))
              coarsityElem(sxx)(sxy)(syy)(l(0.04f))
            }))
          )))))) >> write2DSeq
        )) >> join >> map(padEmpty(4))
      )))
  }

  object omp { // and plain C
    val harrisSeqWrite: Expr = gen.harrisSeqWrite(let)

    private val circularBuffer: Expr = DSL.circularBuffer

    val harrisBuffered: Expr = gen.harrisBuffered(circularBuffer)
  }

  private val sumVec = reduce(add)(vectorFromScalar(l(0.0f)))
  private val dotWeightsVec = fun(weights => fun(input =>
    zip(map(vectorFromScalar, weights), input) |> map(mulT) |> sumVec
  ))

  def nModFun(m: Nat, f: Nat => Expr): Expr = {
    import arithexpr.arithmetic._
    nFun(RangeAdd(0, PosInf, m), f)
  }

  object ocl {
    import rise.OpenCL.DSL._

    private val letGlobal = fun(k => fun(x => toGlobal(x) |> let(k)))

    val harrisSeqWrite: Expr = gen.harrisSeqWrite(letGlobal)

    private val circularBuffer: Expr = oclCircularBuffer(AddressSpace.Global)
    private val registerRotation: Expr = oclRotateValues(AddressSpace.Private)

    val harrisBuffered: Expr = gen.harrisBuffered(circularBuffer)

    def harrisSplitPar(strip: Int, v: Int, innerHarris: Expr): Expr =
      nModFun(strip, h => nModFun(v, w => fun(
        (3`.`(h+4)`.`w`.`f32) ->: (h`.`w`.`f32)
      )(input => input |>
        transpose >> slide(strip+4)(strip) >> mapGlobal(
          transpose >> innerHarris(strip)(w)
        ) >> join
      )))

    def harrisVecUnaligned(v: Int): Expr =
      nFun(h => nModFun(v, w => fun(
        (3`.`(h+4)`.`w`.`f32) ->: (h`.`w`.`f32)
      )(input => input |>
        map(map(asVectorAligned(v))) >>
        transpose >> map(transpose) >> // H.W+2.3.<v>f
        map(map(dotWeightsVec(larr_f32(Seq(0.299f, 0.587f, 0.114f))))) >>
        mapSeq(write1DSeq >> asScalar >> padEmpty(2)) >>
        toGlobal >> let( // H.(W+2)v.f
        slide(3)(1) >> mapSeq( // 3.(W+2)v.f
          map(slideVectors(v) >> slide(3)(v)) >> transpose >> // W.3.3.<v>f
          mapSeq(fun(nbh => makeArray(2)(
            dotWeightsVec(join(sobelXWeights2d), join(nbh)),
            dotWeightsVec(join(sobelYWeights2d), join(nbh))
          ) |> mapSeqUnroll(id))) >> transpose >>
          map(asScalar >> padEmpty(2)) // 2.Wv.f
        ) >> toGlobal >> let( // H.2.Wv.f
        slide(3)(1) >> mapSeq( // 3.2.Wv.f
          map(map(slideVectors(v) >> slide(3)(v))) >> // 3.2.W.3.<v>f
          map(transpose) >> transpose >> map(map(transpose)) >> // W.3.3.2.<v>f
          map(fun(ixiy =>
            ixiy |> map(map(fun(p => (p `@` lidx(0, 2)) * (p `@` lidx(0, 2)))))
            |> fun(ixx =>
            ixiy |> map(map(fun(p => (p `@` lidx(0, 2)) * (p `@` lidx(1, 2)))))
            |> fun(ixy =>
            ixiy |> map(map(fun(p => (p `@` lidx(1, 2)) * (p `@` lidx(1, 2)))))
            |> fun(iyy =>
            // ^ 3.3.<v>f
            ixx |> fun(nbh => sumVec(join(nbh))) >> toPrivate >> let(fun(sxx =>
            ixy |> fun(nbh => sumVec(join(nbh))) >> toPrivate >> let(fun(sxy =>
            iyy |> fun(nbh => sumVec(join(nbh))) >> toPrivate >> let(fun(syy =>
              // ^ <v>f
              coarsityElem(sxx)(sxy)(syy)(vectorFromScalar(l(0.04f)))
            )))))))))
          )) >> write1DSeq >> asScalar // W.f
        )))
      )))

    def harrisBufferedVecUnaligned(bLines: Int, v: Int): Expr =
      nFun(h => nModFun(v, w => fun(
        (3`.`(h+4)`.`w`.`f32) ->: (h`.`w`.`f32)
      )(input => input |>
        map(map(asVectorAligned(v))) >>
        transpose >> map(transpose) >> // H.W.3.<v>f
        map(map(dotWeightsVec(larr_f32(Seq(0.299f, 0.587f, 0.114f))))) >>
        circularBuffer(bLines)(3)(write1DSeq >> asScalar >> padEmpty(2)) >>
        circularBuffer(bLines)(3)( // 3.W.f
          map(slideVectors(v) >> slide(3)(v)) >> transpose >> // W.3.3.<v>f
          mapSeq(fun(nbh =>
            join(nbh) |> mapSeqUnroll(id) |> toPrivate |>
            let(fun(jnbh => pair(
              dotWeightsVec(join(sobelXWeights2d))(jnbh),
              dotWeightsVec(join(sobelYWeights2d))(jnbh)
            )))
          )) >> unzip >> // (W.<v>f x W.<v>f)
          mapFst(asScalar >> padEmpty(2)) >> mapSnd(asScalar >> padEmpty(2))
        ) >> // H.3.(W.f x W.f)
        iterateStream( // 3.(W.f x W.f)
          map(fun(p => zipND(2)(
            fst(p) |> slideVectors(v) >> slide(3)(v),
            snd(p) |> slideVectors(v) >> slide(3)(v)
          ))) >> transpose >> // W.3.3.(<v>f x <v>f)
          mapSeq(mapSeqUnroll(mapSeqUnroll(id)) >> toPrivate >>
            let(fun(ixiy =>
            ixiy |> map(map(fun(p => fst(p) * fst(p)))) |> fun(ixx =>
            ixiy |> map(map(fun(p => fst(p) * snd(p)))) |> fun(ixy =>
            ixiy |> map(map(fun(p => snd(p) * snd(p)))) |> fun(iyy =>
            // ^ 3.3.<v>f
            ixx |> fun(nbh => sumVec(join(nbh))) >> toPrivate >> let(fun(sxx =>
            ixy |> fun(nbh => sumVec(join(nbh))) >> toPrivate >> let(fun(sxy =>
            iyy |> fun(nbh => sumVec(join(nbh))) >> toPrivate >> let(fun(syy =>
            // ^ <v>f
            coarsityElem(sxx)(sxy)(syy)(vectorFromScalar(l(0.04f)))
            )))))))))
          ))) >> asScalar // W.f
        )
      )))

    def shuffle(v: Int): Expr =
      asScalar >> take(v+2) >> slideVectors(v)
    def harrisBufferedVecAligned(bLines: Int, v: Int): Expr =
      nFun(h => nModFun(v, w => fun(
        (3`.`(h+4)`.`w`.`f32) ->: (h`.`w`.`f32)
      )(input => input |>
        map(map(asVectorAligned(v))) >>
        transpose >> map(transpose) >>
        map(map(dotWeightsVec(larr_f32(Seq(0.299f, 0.587f, 0.114f))))) >>
        circularBuffer(bLines)(3)(write1DSeq >> padEmpty(1)) >>
        circularBuffer(bLines)(3)(
          map(slide(2)(1)) >> transpose >>
          mapSeq(fun(nbh =>
            nbh |> mapSeqUnroll(mapSeqUnroll(id)) |> toPrivate |>
            map(shuffle(v)) >> join >>
            let(fun(jnbh => pair(
              dotWeightsVec(join(sobelXWeights2d), jnbh),
              dotWeightsVec(join(sobelYWeights2d), jnbh)
            )))
          )) >> unzip >>
          mapFst(padEmpty(1)) >> mapSnd(padEmpty(1))
        ) >>
        iterateStream( // 3.(W.<v>f x W.<v>f)
          map(fun(p =>
            zip(fst(p), snd(p)) |> slide(2)(1) // W.2.(<v>f x <v>f)
          )) >> transpose >> // W.3.2.(<v>f x <v>f)
          mapSeq(mapSeqUnroll(mapSeqUnroll(id)) >> toPrivate >>
            map(unzip >> fun(p =>
              zip(shuffle(v)(fst(p)), shuffle(v)(snd(p)))
            )) >> let(fun(ixiy =>
            ixiy |> map(map(fun(p => fst(p) * fst(p)))) |> fun(ixx =>
            ixiy |> map(map(fun(p => fst(p) * snd(p)))) |> fun(ixy =>
            ixiy |> map(map(fun(p => snd(p) * snd(p)))) |> fun(iyy =>
            // ^ 3.3.<v>f
            sumVec(join(ixx)) |> toPrivate >> let(fun(sxx =>
            sumVec(join(ixy)) |> toPrivate >> let(fun(sxy =>
            sumVec(join(iyy)) |> toPrivate >> let(fun(syy =>
            coarsityElem(sxx)(sxy)(syy)(vectorFromScalar(l(0.04f)))
            )))))))))
          ))) >> asScalar
        )
      )))

    def harrisBufferedRegRotVecAligned(bLines: Int, v: Int): Expr =
      nFun(h => nModFun(v, w => fun(
        (3`.`(h+4)`.`w`.`f32) ->: (h`.`w`.`f32)
      )(input => input |>
        map(map(asVectorAligned(v))) >>
        transpose >> map(transpose) >>
        map(map(dotWeightsVec(larr_f32(Seq(0.299f, 0.587f, 0.114f))))) >>
        circularBuffer(bLines)(3)(write1DSeq >> padEmpty(1)) >>
        circularBuffer(bLines)(3)(
          transpose >> map(fun(nbh => pair(
            dotWeightsVec(sobelXWeightsV, nbh),
            dotWeightsVec(sobelYWeightsV, nbh)
          ))) >>
          registerRotation(2)(id) >>
          iterateStream(unzip >> fun(nbh => pair(
            dotWeightsVec(sobelXWeightsH, shuffle(v)(fst(nbh))),
            dotWeightsVec(sobelYWeightsH, shuffle(v)(snd(nbh)))
          ))) >> unzip >>
          mapFst(padEmpty(1)) >> mapSnd(padEmpty(1))
        ) >>
        iterateStream( // 3.(W.<v>f x W.<v>f)
          map(fun(p => zip(fst(p), snd(p)))) >> // W.(<v>f x <v>f)
          transpose >> // W.3.(<v>f x <v>f)
          map(fun(nbh => makeArray(3)(
            map(fun(p => fst(p) * fst(p)), nbh),
            map(fun(p => fst(p) * snd(p)), nbh),
            map(fun(p => snd(p) * snd(p)), nbh),
          ) |> map(sumVec))) >> // W.3.<v>f
          registerRotation(2)(mapSeqUnroll(id)) >> // W.2.3.<v>f
          iterateStream(transpose >> // 3.2<v>f
            mapSeqUnroll(shuffle(v) >> sumVec) >> // 3.<v>f
            toPrivate >> let(fun(sxys => {
              val sxx = sxys `@` lidx(0, 3)
              val sxy = sxys `@` lidx(1, 3)
              val syy = sxys `@` lidx(2, 3)
              coarsityElem(sxx)(sxy)(syy)(vectorFromScalar(l(0.04f)))
            }))
          ) >> asScalar
        )
      )))
  }
}
