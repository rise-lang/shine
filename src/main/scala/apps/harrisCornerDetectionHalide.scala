package apps

import rise.core._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.DSL.Type._
import rise.core.types._
import rise.core.types.DataType._
import HighLevelConstructs._

// This version of Harris uses Halide's version as reference
// used in the CGO'21 paper: https://ieeexplore.ieee.org/abstract/document/9370337/
// in Halide: https://github.com/halide/Halide/blob/e8acdea/apps/harris
// in PolyMage: https://bitbucket.org/udayb/polymage/src/e28327c/sandbox/apps/python/img_proc/harris
// FIXME: PolyMage's algorithm is different
object harrisCornerDetectionHalide {
  private val C2D = separableConvolution2D
  private val mulT = C2D.mulT
  private val dot = C2D.dot

  private def larr_f32(s: Seq[Float]): ToBeTyped[Expr] = {
    larr(s.map(semantics.FloatData))
  }

  val gray: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (3`.`h`.`w`.`f32) ->: (h`.`w`.`f32)
  )(input => input |>
    transpose >> map(transpose) >>
    map(map(dot(larr_f32(Seq(0.299f, 0.587f, 0.114f)))))
  ))

  val sobelXWeights2d: ToBeTyped[Expr] = C2D.weights2d(1.0f / 12.0f, Seq(
    Seq(-1, 0, +1),
    Seq(-2, 0, +2),
    Seq(-1, 0, +1)
  ))
  val sobelXWeightsV: ToBeTyped[Expr] = C2D.weights1d(1.0f, Seq(
    1, 2, 1
  ))
  val sobelXWeightsH: ToBeTyped[Expr] = C2D.weights1d(1.0f / 12.0f, Seq(
    -1, 0, +1
  ))

  val sobelYWeights2d: ToBeTyped[Expr] = C2D.weights2d(1.0f / 12.0f, Seq(
    Seq(-1, -2, -1),
    Seq( 0,  0,  0),
    Seq( 1,  2,  1)
  ))
  val sobelYWeightsV: ToBeTyped[Expr] = C2D.weights1d(1.0f, Seq(
    -1, 0, +1
  ))
  val sobelYWeightsH: ToBeTyped[Expr] = C2D.weights1d(1.0f / 12.0f, Seq(
    1, 2, 1
  ))

  val conv3x3: ToBeTyped[Expr] = fun(3`.`3`.`f32)(weights =>
    depFun((h: Nat, w: Nat) => fun(
      ((h+2)`.`(w+2)`.`f32) ->: (h`.`w`.`f32)
    )(input => input |>
      slide2D(3, 1) >>
      map(map(fun(nbh => dot(join(weights))(join(nbh)))))
    ))
  )
  val sobelX: ToBeTyped[Expr] = conv3x3(sobelXWeights2d)
  val sobelY: ToBeTyped[Expr] = conv3x3(sobelYWeights2d)

  val mul = depFun((h: Nat, w: Nat) => fun(
    (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32)
  )((a, b) =>
    zipND(2)(a, b) |> map(map(mulT))
  ))

  val sum: ToBeTyped[Expr] = reduce(add)(lf32(0.0f))
  val sum3x3: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    ((h+2)`.`(w+2)`.`f32) ->: (h`.`w`.`f32)
  )(input => input |>
    slide2D(3, 1) >>
    map(map(fun(nbh => sum(join(nbh)))))
  ))

  // halide: det + trace + output
  val coarsityElem = fun(sxx => fun(sxy => fun(syy => fun(kappa => {
    val det = sxx * syy - sxy * sxy
    val trace = sxx + syy
    det - kappa * trace * trace
  }))))
  val coarsity = depFun((h: Nat, w: Nat) => fun(
    (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32)
  )((sxx, sxy, syy) =>
    zipND(2)(sxx, zipND(2)(sxy, syy)) |> map(map(fun { s =>
      val sxx = fst(s)
      val sxy = fst(snd(s))
      val syy = snd(snd(s))
      coarsityElem(sxx)(sxy)(syy)(lf32(0.04f))
    }))
  ))

  // note: the output is strided:
  // it only contains w-4 meaningful values per line
  // so 4 meaningless values at the end of each line
  // this way if the input is dividable by a vector width,
  // so is the output
  def harris(hMod: Int, wMod: Int): ToBeTyped[Expr] =
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
    def harrisSeqWrite(letMem: ToBeTyped[Expr]): ToBeTyped[Expr] =
      depFun((h: Nat, w: Nat) => fun(
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
      ))

    def harrisBuffered(circularBuffer: ToBeTyped[Expr]): ToBeTyped[Expr] =
      depFun((h: Nat, w: Nat) => fun(
        (3`.`(h+4)`.`w`.`f32) ->: (h`.`w`.`f32)
      )(input => input |>
        transpose >> map(transpose) >>
        // map(map(dot(larr_f32(Seq(0.299f, 0.587f, 0.114f))))) >>
        circularBuffer(3)(3)(
          map(dot(larr_f32(Seq(0.299f, 0.587f, 0.114f)))) >>
          write1DSeq) >>
        circularBuffer(3)(3)(
          map(slide(3)(1)) >> transpose >>
          map(fun(nbh => makePair(
            dot(join(sobelXWeights2d))(join(nbh)))(
            dot(join(sobelYWeights2d))(join(nbh))
          ))) >> write1DSeq >> unzip
        ) >> // H.3.(W.f x W.f)
        iterateStream(
          map(fun(p => zip(fst(p))(snd(p)))) >> fun(ixiy => // 3.W.(f x f)
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
              coarsityElem(sxx)(sxy)(syy)(lf32(0.04f))
            }))
          )))))) >> write2DSeq
        )) >> join >> map(padEmpty(4))
      ))
  }

  object omp { // and plain C
    private val letStack = fun(k => fun(x => toMem(x) |> letf(k)))

    val harrisSeqWrite: ToBeTyped[Expr] = gen.harrisSeqWrite(letStack)

    private val circularBuffer: ToBeTyped[Expr] = primitives.circularBuffer

    val harrisBuffered: ToBeTyped[Expr] = gen.harrisBuffered(circularBuffer)
  }

  private val sumVec = reduce(add)(vectorFromScalar(lf32(0.0f)))
  private val dotWeightsVec = fun(weights => fun(input =>
    zip(map(vectorFromScalar)(weights))(input) |> map(mulT) |> sumVec
  ))

  def nModFun(m: Nat, f: Nat => ToBeTyped[Expr]): ToBeTyped[Expr] = {
    import arithexpr.arithmetic._
    depFun(RangeAdd(0, PosInf, m), f)
  }

  object ocl {
    import rise.openCL.DSL._
    import rise.openCL.primitives.{oclCircularBuffer, oclRotateValues}

    private val letGlobal = fun(k => fun(x => toGlobal(x) |> letf(k)))

    val harrisSeqWrite: ToBeTyped[Expr] = gen.harrisSeqWrite(letGlobal)

    private val circularBuffer: ToBeTyped[Expr] = oclCircularBuffer(AddressSpace.Global)
    private val registerRotation: ToBeTyped[Expr] = oclRotateValues(AddressSpace.Private)

    val harrisBuffered: ToBeTyped[Expr] = gen.harrisBuffered(circularBuffer)

    def harrisSplitPar(strip: Int, v: Int, innerHarris: ToBeTyped[Expr]): ToBeTyped[Expr] =
      nModFun(strip, h => nModFun(v, w => fun(
        (3`.`(h+4)`.`w`.`f32) ->: (h`.`w`.`f32)
      )(input => input |>
        transpose >> slide(strip+4)(strip) >> mapGlobal(
          transpose >> innerHarris(strip)(w)
        ) >> join
      )))

    def harrisTilePar(tileX: Int, tileY: Int, mapPar: Int => ToBeTyped[Expr],
                      innerHarris: ToBeTyped[Expr]): ToBeTyped[Expr] =
      depFun((h: Nat, w: Nat) => fun(
        (3`.`(h+4)`.`w`.`f32) ->: (h`.`w`.`f32)
      )(input => input |>
        slide2D(tileY+4, tileY, tileX+4, tileX) >>
        mapPar(1)(mapPar(0)(
          map(transpose) >> transpose >>
          innerHarris(tileY)(tileX)
        )) >> unslide2D >> map(padEmpty(4))
      ))

    def harrisTileShiftInwardsPar(tileX: Int, tileY: Int, mapPar: Int => ToBeTyped[Expr],
                                  innerHarris: ToBeTyped[Expr]): ToBeTyped[Expr] =
      depFun((h: Nat, w: Nat) => fun(
        (3`.`(h+4)`.`w`.`f32) ->: (h`.`w`.`f32)
      )(input => input |>
        transpose >> map(transpose) >> // H.W.3.
        tileShiftInwards(tileY)(mapPar(1)( // tY.W.3.
          transpose >> // W.tY.3.
          tileShiftInwards(tileX)(mapPar(0)( // tX.tY.3.
            map(transpose) >> transpose >> map(transpose) >> // 3.tY.tX.
            innerHarris(tileY)(tileX) >> // tY.tX.
            transpose // tX.tY.
          )) >> // W.tY.
          transpose // ty.W.
        )) >> // H.W.
        map(padEmpty(4))
      ))

    def harrisVecUnaligned(v: Int): ToBeTyped[Expr] =
      depFun((h: Nat) => nModFun(v, w => fun(
        (3`.`(h+4)`.`w`.`f32) ->: (h`.`w`.`f32)
      )(input => input |>
        map(map(asVectorAligned(v))) >>
        transpose >> map(transpose) >> // H.W+2.3.<v>f
        // map() >>
        mapSeq(
          map(dotWeightsVec(larr_f32(Seq(0.299f, 0.587f, 0.114f)))) >>
          write1DSeq >> asScalar >> padEmpty(2)
        ) >>
        toGlobal >> letf( // H.(W+2)v.f
        slide(3)(1) >> mapSeq( // 3.(W+2)v.f
          map(slideVectors(v) >> slide(3)(v)) >> transpose >> // W.3.3.<v>f
          mapSeq(fun(nbh => makeArray(2)(
            dotWeightsVec(join(sobelXWeights2d), join(nbh)))(
            dotWeightsVec(join(sobelYWeights2d), join(nbh))
          ) |> mapSeqUnroll(id))) >> transpose >>
          map(asScalar >> padEmpty(2)) // 2.Wv.f
        ) >> toGlobal >> letf( // H.2.Wv.f
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
            ixx |> fun(nbh => sumVec(join(nbh))) >> toPrivate >> letf(fun(sxx =>
            ixy |> fun(nbh => sumVec(join(nbh))) >> toPrivate >> letf(fun(sxy =>
            iyy |> fun(nbh => sumVec(join(nbh))) >> toPrivate >> letf(fun(syy =>
              // ^ <v>f
              coarsityElem(sxx)(sxy)(syy)(vectorFromScalar(lf32(0.04f)))
            )))))))))
          )) >> write1DSeq >> asScalar // W.f
        )))
      )))

    // limitations:
    // - v <= 4
    // - if toMem = toPrivate, loop unrolling can fail due to blowup
    def harrisVecUnaligned2(v: Int,
                            mapPar: Int => ToBeTyped[Expr],
                            toMem: ToBeTyped[Expr]): ToBeTyped[Expr] =
      depFun((h: Nat) => nModFun(v, w => fun(
        (3`.`(h+4)`.`(w+4)`.`f32) ->: (h`.`w`.`f32)
      )(input => input |>
        map(map(asVectorAligned(v))) >>
        transpose >> map(transpose) >>
        mapPar(1)(
          mapPar(0)(dotWeightsVec(larr_f32(Seq(0.299f, 0.587f, 0.114f)))) >>
          asScalar >> padEmpty(2)
        ) >> toMem >> letf(
        slide(3)(1) >> mapPar(1)(
          map(slideVectors(v) >> slide(3)(v)) >> transpose >>
          mapPar(0)(fun(nbh => makeArray(2)(
            dotWeightsVec(join(sobelXWeights2d), join(nbh)))(
            dotWeightsVec(join(sobelYWeights2d), join(nbh))
          ) |> mapSeqUnroll(id))) >> transpose >>
          map(asScalar)
        ) >> toMem >> letf(
        slide(3)(1) >> mapPar(1)(
          map(map(dropLast(2) >> slideVectors(v) >> slide(3)(v))) >>
          map(transpose) >> transpose >> map(map(transpose)) >>
          mapPar(0)(fun(ixiy =>
            ixiy |> map(map(fun(p => (p `@` lidx(0, 2)) * (p `@` lidx(0, 2)))))
            |> fun(ixx =>
            ixiy |> map(map(fun(p => (p `@` lidx(0, 2)) * (p `@` lidx(1, 2)))))
            |> fun(ixy =>
            ixiy |> map(map(fun(p => (p `@` lidx(1, 2)) * (p `@` lidx(1, 2)))))
            |> fun(iyy =>
            // ^ 3.3.<v>f
            ixx |> fun(nbh => sumVec(join(nbh))) >> toPrivate >> letf(fun(sxx =>
            ixy |> fun(nbh => sumVec(join(nbh))) >> toPrivate >> letf(fun(sxy =>
            iyy |> fun(nbh => sumVec(join(nbh))) >> toPrivate >> letf(fun(syy =>
              // ^ <v>f
              coarsityElem(sxx)(sxy)(syy)(vectorFromScalar(lf32(0.04f)))
            )))))))))
          )) >> asScalar // W.f
        )))
      )))

    def harrisBufferedVecUnaligned(bLines: Int, v: Int): ToBeTyped[Expr] =
      depFun((h: Nat) => nModFun(v, w => fun(
        (3`.`(h+4)`.`w`.`f32) ->: (h`.`w`.`f32)
      )(input => input |>
        map(map(asVectorAligned(v))) >>
        transpose >> map(transpose) >> // H.W.3.<v>f
        circularBuffer(bLines)(3)(
          map(dotWeightsVec(larr_f32(Seq(0.299f, 0.587f, 0.114f)))) >>
          write1DSeq >> asScalar >> padEmpty(2)
        ) >>
        circularBuffer(bLines)(3)( // 3.W.f
          map(slideVectors(v) >> slide(3)(v)) >> transpose >> // W.3.3.<v>f
          mapSeq(fun(nbh =>
            join(nbh) |> mapSeqUnroll(id) |> toPrivate |>
            letf(jnbh => makePair(
              dotWeightsVec(join(sobelXWeights2d))(jnbh))(
              dotWeightsVec(join(sobelYWeights2d))(jnbh)
            ))
          )) >> unzip >> // (W.<v>f x W.<v>f)
          mapFst(asScalar >> padEmpty(2)) >> mapSnd(asScalar >> padEmpty(2))
        ) >> // H.3.(W.f x W.f)
        iterateStream( // 3.(W.f x W.f)
          map(fun(p => zipND(2)(
            fst(p) |> slideVectors(v) >> slide(3)(v),
            snd(p) |> slideVectors(v) >> slide(3)(v)
          ))) >> transpose >> // W.3.3.(<v>f x <v>f)
          mapSeq(mapSeqUnroll(mapSeqUnroll(id)) >> toPrivate >>
            letf(ixiy =>
            ixiy |> map(map(fun(p => fst(p) * fst(p)))) |> fun(ixx =>
            ixiy |> map(map(fun(p => fst(p) * snd(p)))) |> fun(ixy =>
            ixiy |> map(map(fun(p => snd(p) * snd(p)))) |> fun(iyy =>
            // ^ 3.3.<v>f
            ixx |> fun(nbh => sumVec(join(nbh))) >> toPrivate >> letf(sxx =>
            ixy |> fun(nbh => sumVec(join(nbh))) >> toPrivate >> letf(sxy =>
            iyy |> fun(nbh => sumVec(join(nbh))) >> toPrivate >> letf(syy =>
            // ^ <v>f
            coarsityElem(sxx)(sxy)(syy)(vectorFromScalar(lf32(0.04f)))
            ))))))
          )) >> asScalar // W.f
        )
      )))

    def shuffle(v: Int): ToBeTyped[Expr] =
      asScalar >> take(v+2) >> slideVectors(v)
    def harrisBufferedVecAligned(bLines: Int, v: Int): ToBeTyped[Expr] =
      depFun((h: Nat) => nModFun(v, w => fun(
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
            letf(jnbh => makePair(
              dotWeightsVec(join(sobelXWeights2d), jnbh))(
              dotWeightsVec(join(sobelYWeights2d), jnbh)
            ))
          )) >> unzip >>
          mapFst(padEmpty(1)) >> mapSnd(padEmpty(1))
        ) >>
        iterateStream( // 3.(W.<v>f x W.<v>f)
          map(fun(p =>
            zip(fst(p))(snd(p)) |> slide(2)(1) // W.2.(<v>f x <v>f)
          )) >> transpose >> // W.3.2.(<v>f x <v>f)
          mapSeq(mapSeqUnroll(mapSeqUnroll(id)) >> toPrivate >>
            map(unzip >> fun(p =>
              zip(shuffle(v)(fst(p)))(shuffle(v)(snd(p)))
            )) >> letf(ixiy =>
            ixiy |> map(map(fun(p => fst(p) * fst(p)))) |> fun(ixx =>
            ixiy |> map(map(fun(p => fst(p) * snd(p)))) |> fun(ixy =>
            ixiy |> map(map(fun(p => snd(p) * snd(p)))) |> fun(iyy =>
            // ^ 3.3.<v>f
            sumVec(join(ixx)) |> toPrivate >> letf(sxx =>
            sumVec(join(ixy)) |> toPrivate >> letf(sxy =>
            sumVec(join(iyy)) |> toPrivate >> letf(syy =>
            coarsityElem(sxx)(sxy)(syy)(vectorFromScalar(lf32(0.04f)))
            ))))))
          )) >> asScalar
        )
      )))

    def harrisBufferedRegRotVecAligned(bLines: Int, v: Int): ToBeTyped[Expr] =
      depFun((h: Nat) => nModFun(v, w => fun(
        (3`.`(h+4)`.`w`.`f32) ->: (h`.`w`.`f32)
      )(input => input |>
        map(map(asVectorAligned(v))) >>
        transpose >> map(transpose) >>
        map(map(dotWeightsVec(larr_f32(Seq(0.299f, 0.587f, 0.114f))))) >>
        circularBuffer(bLines)(3)(write1DSeq >> padEmpty(1)) >>
        circularBuffer(bLines)(3)(
          transpose >> map(fun(nbh => makePair(
            dotWeightsVec(sobelXWeightsV, nbh))(
            dotWeightsVec(sobelYWeightsV, nbh)
          ))) >>
          registerRotation(2)(id) >>
          iterateStream(unzip >> fun(nbh => makePair(
            dotWeightsVec(sobelXWeightsH, shuffle(v)(fst(nbh))))(
            dotWeightsVec(sobelYWeightsH, shuffle(v)(snd(nbh)))
          ))) >> unzip >>
          mapFst(padEmpty(1)) >> mapSnd(padEmpty(1))
        ) >>
        iterateStream( // 3.(W.<v>f x W.<v>f)
          map(fun(p => zip(fst(p))(snd(p)))) >> // W.(<v>f x <v>f)
          transpose >> // W.3.(<v>f x <v>f)
          map(fun(nbh => makeArray(3)(
            map(fun(p => fst(p) * fst(p)))(nbh))(
            map(fun(p => fst(p) * snd(p)))(nbh))(
            map(fun(p => snd(p) * snd(p)))(nbh)
          ) |> map(sumVec))) >> // W.3.<v>f
          registerRotation(2)(mapSeqUnroll(id)) >> // W.2.3.<v>f
          iterateStream(transpose >> // 3.2<v>f
            mapSeqUnroll(shuffle(v) >> sumVec) >> // 3.<v>f
            toPrivate >> letf(sxys => {
              val sxx = sxys `@` lidx(0, 3)
              val sxy = sxys `@` lidx(1, 3)
              val syy = sxys `@` lidx(2, 3)
              coarsityElem(sxx)(sxy)(syy)(vectorFromScalar(lf32(0.04f)))
            })
          ) >> asScalar
        )
      )))

/*
    def greyParVec(v: Int): ToBeTyped[Expr] =
      depFun((h: Nat) => nModFun(v, w => fun(
        (3`.`(h+4)`.`w`.`f32) ->: ((h+4)`.`w`.`f32)
      )(input => input |>
        map(map(asVectorAligned(v))) >>
        transpose >> map(transpose) >> // H.W+2.3.<v>f
        map(map(dotWeightsVec(larr_f32(Seq(0.299f, 0.587f, 0.114f))))) >>
        mapGlobal(write1DSeq >> asScalar) // padEmpty(2))
      )))
*/
    // following variants are used for comparison to Lift

    def grayPar: ToBeTyped[Expr] =
      depFun((h: Nat, w: Nat) => fun(
        (3`.`(h+4)`.`w`.`f32) ->: ((h+4)`.`w`.`f32)
      )(input =>
        gray(h+4)(w)(input) |> mapGlobal(write1DSeq)
      ))

    def sobelXPar: ToBeTyped[Expr] =
      depFun((h: Nat, w: Nat) => fun(
        ((h+4)`.`w`.`f32) ->: ((h+2)`.`(w-2)`.`f32)
      )(gray =>
        sobelX(h+2)(w-2)(gray) |> mapGlobal(write1DSeq)
      ))

    def sobelYPar: ToBeTyped[Expr] =
      depFun((h: Nat, w: Nat) => fun(
        ((h+4)`.`w`.`f32) ->: ((h+2)`.`(w-2)`.`f32)
      )(gray =>
        sobelY(h+2)(w-2)(gray) |> mapGlobal(write1DSeq)
      ))

    def mulPar: ToBeTyped[Expr] =
      depFun((h: Nat, w: Nat) => fun(
        ((h+2)`.`(w-2)`.`f32) ->: ((h+2)`.`(w-2)`.`f32) ->: ((h+2)`.`(w-2)`.`f32)
      )((ix, iy) =>
        mul(h+2)(w-2)(ix)(iy) |> mapGlobal(write1DSeq)
      ))

    def sum3x3Par: ToBeTyped[Expr] =
      depFun((h: Nat, w: Nat) => fun(
        ((h+2)`.`(w-2)`.`f32) ->: (h`.`(w-4)`.`f32)
      )(input =>
        sum3x3(h)(w-4)(input) |> mapGlobal(write1DSeq)
      ))

    def coarsityPadPar: ToBeTyped[Expr] =
      depFun((h: Nat, w: Nat) => fun(
        (h`.`(w-4)`.`f32) ->: (h`.`(w-4)`.`f32) ->: (h`.`(w-4)`.`f32) ->: (h`.`w`.`f32)
      )((sxx, sxy, syy) =>
        coarsity(h)(w-4)(sxx)(sxy)(syy) |> mapGlobal(write1DSeq) |> map(padEmpty(4))
      ))
  }
}
