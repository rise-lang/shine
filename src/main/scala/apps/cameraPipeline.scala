package apps

import rise.core._
import rise.core.types._
import rise.core.types.DataType._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.DSL.Type._
import HighLevelConstructs._

// in Halide: https://github.com/halide/Halide/blob/e8acdea/apps/camera_pipe
// in PolyMage: https://bitbucket.org/udayb/polymage/src/e28327c/sandbox/apps/python/img_proc/campipe
// FIXME: check if PolyMage's algorithm is different?
object cameraPipeline {
  // iN -> iN -> uN
  val abs_diff: ToBeTyped[Expr] = foreignFun("abs_diff_i16", i16 ->: i16 ->: u16)
  def clamp(dt: DataType): ToBeTyped[Expr] =
    foreignFun(s"clamp_${dt}", dt ->: dt ->: dt ->: dt)
  val max: ToBeTyped[Expr] = foreignFun("max_i16", i16 ->: i16 ->: i16)
  val pow: ToBeTyped[Expr] = foreignFun("pow_f32", f32 ->: f32 ->: f32)

  // average two positive values rounding up
  val avg: ToBeTyped[Expr] =
    depFun((dt: DataType) => depFun((compute_dt: DataType) =>
      fun(dt ->: dt ->: dt)((a, b) => {
    val cdt = fun(a => cast(a) :: compute_dt)
    // FIXME: avoid literal casts?
    cast((cdt(a) + cdt(b) + cdt(l(1))) / cdt(l(2))) :: dt
  })))

  case class Image(
    xBeg: Nat, xEnd: Nat,
    yBeg: Nat, yEnd: Nat,
    expr: ToBeTyped[Expr]
  )

  def letImage(img: Image, f: Image => ToBeTyped[Expr]): ToBeTyped[Expr] =
    img.expr |> fun(x => f(Image(img.xBeg, img.xEnd, img.yBeg, img.yEnd, x)))

  def mapImage(img: Image, f: ToBeTyped[Expr]): Image =
    Image(img.xBeg, img.xEnd, img.yBeg, img.yEnd, img.expr |> map(map(f)))

  def zipImage(a: Image, b: Image): Image = {
    val Seq(ai, bi) = intersectImages(Seq(a, b))
    Image(ai.xBeg, ai.xEnd, ai.yBeg, ai.yEnd, zipND(2)(ai.expr, bi.expr))
  }

  // point-wise avg
  def interpolate(img: Image): Image = mapImage(img, fun(2`.`i16)(x =>
    avg(i16)(i32)(x `@` lidx(0, 2), x `@` lidx(1, 2))
  ))
  // point-wise abs_diff
  def pointAbsDiff(img: Image): Image = mapImage(img, fun(2`.`i16)(x =>
    abs_diff(x `@` lidx(0, 2), x `@` lidx(1, 2))
  ))

  // assumptions: newBeg >= beg, newEnd <= end, newEnd >= newBeg
  def slice(beg: Nat, end: Nat, newBeg: Nat, newEnd: Nat): ToBeTyped[Expr] = {
    drop(newBeg - beg) >> take(newEnd - newBeg)
  }

  def intersectImages(images: Seq[Image]): Seq[Image] = {
    import arithexpr.arithmetic.ArithExpr.Math.Max
    import arithexpr.arithmetic.ArithExpr.Math.Min

    val xBeg = images.map(_.xBeg).reduce(Max)
    val yBeg = images.map(_.yBeg).reduce(Max)
    val xEnd = images.map(_.xEnd).reduce(Min)
    val yEnd = images.map(_.yEnd).reduce(Min)

    images.map(img => {
      val xSlice = slice(img.xBeg, img.xEnd, xBeg, xEnd)
      val ySlice = slice(img.yBeg, img.yEnd, yBeg, yEnd)
      val sliced = img.expr |> map(xSlice) |> ySlice
      Image(xBeg, xEnd, yBeg, yEnd, sliced)
    })
  }

  def stencilCollect(offsets: Seq[(Int, Int)], img: Image): Image = {
    val n = offsets.length
    val x_min = offsets.map(_._1).min
    val x_max = offsets.map(_._1).max
    val x_range = x_max - x_min + 1
    val y_min = offsets.map(_._2).min
    val y_max = offsets.map(_._2).max
    val y_range = y_max - y_min + 1

    mapImage(
      Image(
        img.xBeg - x_min, img.xEnd - x_max,
        img.yBeg - y_min, img.yEnd - y_max,
        img.expr |> slide2D(y_range, 1, x_range, 1)
      ),
      fun(nbh =>
        offsets.foldLeft(makeArray(n): ToBeTyped[Expr])({ case (e, offset) =>
          e(nbh `@`
            lidx(offset._2 - y_min, y_range) `@`
            lidx(offset._1 - x_min, x_range))
        })
      )
    )
  }

  def interleaveX: ToBeTyped[Expr] = impl{ dt: DataType => impl{ h: Nat => impl{ w: Nat => fun(
    (h`.`w`.`dt) ->: (h`.`w`.`dt) ->: (h`.`(2*w)`.`dt)
  )((a, b) =>
    generate(fun(i => select(i =:= lidx(0, 2))(a)(b))) |>
    transpose >> map(transpose >> join)
  ) }}}

  def interleaveY: ToBeTyped[Expr] = impl{ dt: DataType => impl{ h: Nat => impl{ w: Nat => fun(
    (h`.`w`.`dt) ->: (h`.`w`.`dt) ->: ((2*h)`.`w`.`dt)
  )((a, b) =>
    generate(fun(i => select(i =:= lidx(0, 2))(a)(b))) |>
    transpose >> join
  ) }}}

  val demosaic: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (4`.`(h+2)`.`(w+2)`.`i16) ->: (3`.`(2*h)`.`(2*w)`.`i16)
  )(deinterleaved => {
    // x_y = the value of channel x at a site in the input of channel y
    // gb = green sites in the blue rows
    // gr = green sites in the red rows
    // | gr r |    | r_gr r_r |   | g_gr g_r |   | b_gr b_r |
    // | b gb | -> | r_b r_gb | , | g_b g_gb | , | b_b b_gb |
    val g_gr = Image(0, w+2, 0, h+2, deinterleaved `@` lidx(0, 4))
    val r_r = Image(0, w+2, 0, h+2, deinterleaved `@` lidx(1, 4))
    val b_b = Image(0, w+2, 0, h+2, deinterleaved `@` lidx(2, 4))
    val g_gb = Image(0, w+2, 0, h+2, deinterleaved `@` lidx(3, 4))

    // first calculate green at the red and blue sites

    def select_interpolation(
      a: Image, ad: Image, b: Image, bd: Image
    ): Image = {
      mapImage(zipImage(zipImage(a, ad), zipImage(b, bd)), fun(ab =>
        `if` (snd(fst(ab)) < snd(snd(ab)))
          .`then` (fst(fst(ab)))
          .`else` (fst(snd(ab)))
      ))
    }

    def correct(a: Image, b: Image, c: Image): Image = {
      mapImage(zipImage(a, zipImage(b, c)), fun(a_bc =>
        fst(a_bc) + (fst(snd(a_bc)) - snd(snd(a_bc)))
      ))
    }

    // try interpolating vertically and horizontally
    // use interpolation in whichever direction had the smallest difference
    letImage(interpolate(stencilCollect(Seq((0, -1), (0, 0)), g_gb)), gv_r =>
    letImage(pointAbsDiff(stencilCollect(Seq((0, -1), (0, 0)), g_gb)), gvd_r =>
    letImage(interpolate(stencilCollect(Seq((1, 0), (0, 0)), g_gr)), gh_r =>
    letImage(pointAbsDiff(stencilCollect(Seq((1, 0), (0, 0)), g_gr)), ghd_r =>

    letImage(select_interpolation(gh_r, ghd_r, gv_r, gvd_r), g_r =>

    letImage(interpolate(stencilCollect(Seq((0, 1), (0, 0)), g_gr)), gv_b =>
    letImage(pointAbsDiff(stencilCollect(Seq((0, 1), (0, 0)), g_gr)), gvd_b =>
    letImage(interpolate(stencilCollect(Seq((-1, 0), (0, 0)), g_gb)), gh_b =>
    letImage(pointAbsDiff(stencilCollect(Seq((-1, 0), (0, 0)), g_gb)), ghd_b =>

    letImage(select_interpolation(gh_b, ghd_b, gv_b, gvd_b), g_b =>

    // next interpolate red at gr by first interpolating,
    // then correcting using the error green would have had if we had
    // interpolated it in the same way (i.e. add the second derivative
    // of the green channel at the same place).
    letImage(correct(
      interpolate(stencilCollect(Seq((-1, 0), (0, 0)), r_r)),
      g_gr,
      interpolate(stencilCollect(Seq((-1, 0), (0, 0)), g_r))
    ), r_gr =>
    letImage(correct(
      interpolate(stencilCollect(Seq((0, 0), (0, -1)), b_b)),
      g_gr,
      interpolate(stencilCollect(Seq((0, 0), (0, -1)), g_b))
    ), b_gr =>
    letImage(correct(
      interpolate(stencilCollect(Seq((0, 0), (0, 1)), r_r)),
      g_gb,
      interpolate(stencilCollect(Seq((0, 0), (0, 1)), g_r))
    ), r_gb =>
    letImage(correct(
      interpolate(stencilCollect(Seq((0, 0), (1, 0)), b_b)),
      g_gb,
      interpolate(stencilCollect(Seq((0, 0), (1, 0)), g_b))
    ), b_gb =>

    // now interpolate diagonally to get red at blue and blue at red.
    // we try both directions (positive and negative diagonals),
    // and use the one with smallest difference.
    // we also correct our interpolations using
    // the second derivative of green at the same sites.
    letImage(correct(
      interpolate(stencilCollect(Seq((0, 0), (-1, 1)), r_r)),
      g_b,
      interpolate(stencilCollect(Seq((0, 0), (-1, 1)), g_r))
    ), rp_b =>
    letImage(pointAbsDiff(stencilCollect(Seq((0, 0), (-1, 1)), r_r)), rpd_b =>
    letImage(correct(
      interpolate(stencilCollect(Seq((-1, 0), (0, 1)), r_r)),
      g_b,
      interpolate(stencilCollect(Seq((-1, 0), (0, 1)), g_r))
    ), rn_b =>
    letImage(pointAbsDiff(stencilCollect(Seq((-1, 0), (0, 1)), r_r)), rnd_b =>

    letImage(select_interpolation(rp_b, rpd_b, rn_b, rnd_b), r_b =>

    letImage(correct(
      interpolate(stencilCollect(Seq((0, 0), (1, -1)), b_b)),
      g_r,
      interpolate(stencilCollect(Seq((0, 0), (1, -1)), g_b))
    ), bp_r =>
    letImage(pointAbsDiff(stencilCollect(Seq((0, 0), (1, -1)), b_b)), bpd_r =>
    letImage(correct(
      interpolate(stencilCollect(Seq((1, 0), (0, -1)), b_b)),
      g_r,
      interpolate(stencilCollect(Seq((1, 0), (0, -1)), g_b))
    ), bn_r =>
    letImage(pointAbsDiff(stencilCollect(Seq((1, 0), (0, -1)), b_b)), bnd_r =>

    letImage(select_interpolation(bp_r, bpd_r, bn_r, bnd_r), b_r => {
      val Seq(
        r_gr_o, r_r_o, r_b_o, r_gb_o,
        g_gr_o, g_r_o, g_b_o, g_gb_o,
        b_gr_o, b_r_o, b_b_o, b_gb_o
      ) = intersectImages(Seq(
        r_gr, r_r, r_b, r_gb,
        g_gr, g_r, g_b, g_gb,
        b_gr, b_r, b_b, b_gb
      ))
      makeArray(3)(
        interleaveY(
          interleaveX(r_gr_o.expr, r_r_o.expr))(
          interleaveX(r_b_o.expr, r_gb_o.expr)))( // r
        interleaveY(
          interleaveX(g_gr_o.expr, g_r_o.expr))(
          interleaveX(g_b_o.expr, g_gb_o.expr)))( // g
        interleaveY(
          interleaveX(b_gr_o.expr, b_r_o.expr))(
          interleaveX(b_b_o.expr, b_gb_o.expr))  // b
      ) // 3.(2*H).(2*W).f
    }))))))))))))))))))))))))
  }))

  val hot_pixel_suppression: ToBeTyped[Expr] =
    depFun((h: Nat, w: Nat) =>
      fun(((h+4)`.`(w+4)`.`i16) ->: (h`.`w`.`i16))(input =>
    mapImage(
      stencilCollect(
        Seq((-2, 0), (2, 0), (0, 0), (0, -2), (0, 2)),
        Image(0, w+4, 0, h+4, input)),
      fun(5`.`i16)(nbh => clamp(i16)(nbh `@` lidx(2, 5), li16(0),
        max(max(nbh `@` lidx(0, 5), nbh `@` lidx(1, 5)),
          max(nbh `@` lidx(3, 5), nbh `@` lidx(4, 5)))
      ))
    ).expr
  ))

  val deinterleave: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    ((2*h)`.`(2*w)`.`i16) ->: (4`.`h`.`w`.`i16)
  )(raw =>
    raw |>
    map(split(2)) >> split(2) >> // h.2.w.2.i16
      map(map(transpose)) >> // h.2.2.w.
      transpose >> map(transpose) >> // 2.2.h.w.
      join // 4.h.w.
  ))

  val color_correct: ToBeTyped[Expr] =
    depFun((h: Nat, w: Nat, hm: Nat, wm: Nat) => fun(
    (3`.`h`.`w`.`i16) ->:
      (hm`.`wm`.`f32) ->: (hm`.`wm`.`f32) ->: f32 ->:
      (3`.`h`.`w`.`i16)
  )((input, matrix_3200, matrix_7000, color_temp) => {
    // get a color matrix by linearly interpolating between two
    // calibrated matrices using inverse kelvin.
    val kelvin = color_temp
    val alpha =
      (lf32(1.0f) / kelvin - lf32(1.0f / 3200)) / lf32(1.0f / 7000 - 1.0f / 3200)
    (
      zipND(2)(matrix_3200, matrix_7000) |>
        map(map(fun(p => p.`1` * alpha + p.`2` * (lf32(1.0f) - alpha)))) >>
          map(map(fun(v => cast(v * lf32(256.0f)) :: i16))) // Q8.8 fixed point
      ) |> fun(matrix =>
        input |> transpose >>
        map(transpose >> map(map(cast :: (i16 ->: i32)))) // H.W.3.i32
        >> map(map(fun(irgb => {
          val ir = irgb `@` lidx(0, 3)
          val ig = irgb `@` lidx(1, 3)
          val ib = irgb `@` lidx(2, 3)
          def m(x: Int, y: Int): ToBeTyped[Expr] =
            cast(matrix `@` lidx(y, hm) `@` lidx(x, wm)) :: i32
          makeArray(3)(
            m(3, 0) + m(0, 0) * ir + m(1, 0) * ig + m(2, 0) * ib)(
            m(3, 1) + m(0, 1) * ir + m(1, 1) * ig + m(2, 1) * ib)(
            m(3, 2) + m(0, 2) * ir + m(1, 2) * ig + m(2, 2) * ib
          )
        }))) >>
        map(map(map(fun(rgb =>
          cast(rgb / li32(256)) :: i16
        ))) >> transpose) >> transpose
    )
  }))

  val apply_curve: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (3`.`h`.`w`.`i16) ->: f32 ->: f32 ->: int ->: int ->: (3`.`h`.`w`.`u8)
  )((input, gamma, contrast, blackLevel, whiteLevel) => {
    // val lutResample = 1 // how much to resample the LUT by when sampling it
    val minRaw = blackLevel // / l(lutResample)
    val maxRaw = whiteLevel // / l(lutResample)

    val invRange = lf32(1.0f) / (cast(maxRaw - minRaw) :: f32)
    val b = lf32(2.0f) - pow(lf32(2.0f), contrast / lf32(100.0f))
    val a = lf32(2.0f) - lf32(2.0f) * b

    // make LUT
    generate(fun(IndexType(1024))(x_ => {
      val x = cast(x_) :: int
      // get a linear luminance in the range 0-1
      val xf = clamp(f32)(
        (cast(x - minRaw) :: f32) * invRange, lf32(0.0f), lf32(1.0f)
      )
      // gamma correct it
      val g = pow(xf, lf32(1.0f) / gamma)
      // apply a piecewise quadratic contrast curve
      val gmo = lf32(1.0f) - g
      val z = `if` (g > lf32(0.5f))
        .`then` (lf32(1.0f) - (a * gmo * gmo + b * gmo))
        .`else` (a * g * g + b * g)
      // convert to 8 bit
      val v = cast(
        clamp(f32)(z * lf32(255.0f) + lf32(0.5f), lf32(0.0f), lf32(255.0f))
      ) :: u8

      // add guard band outside of (minraw, maxRaw]
      `if` (not(x > minRaw))
        .`then`(lu8(0))
        .`else`(
          `if` (x > maxRaw)
            .`then`(lu8(255))
            .`else`(v)
        )
    })) |> fun(curve =>
      input |> map(map(map(fun(p =>
        curve `@` (
          cast(clamp(i16)(p, li16(0), li16(1023))) :: IndexType(1024)
        )
      ))))
    )
  }))

  val blur121: ToBeTyped[Expr] =
    depFun((dt: DataType) => depFun((compute_dt: DataType) => fun(
    (3`.`dt) ->: dt
  )(vs =>
    avg(dt)(compute_dt)(
      avg(dt)(compute_dt)(vs `@` lidx(0, 3), vs `@` lidx(2, 3)),
      vs `@` lidx(1, 3))
  )))

  val sharpen: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (3`.`(h+2)`.`(w+2)`.`u8) ->: f32 ->: (3`.`h`.`w`.`u8)
  )((input, strength) => {
    // convert sharpening strength to 2.5 fixed point.
    // this allows sharpening in the range [0, 4].
    def u8_sat(dt: DataType) = fun(x =>
      cast(clamp(dt)(x, cast(l(0)) :: dt, cast(l(255)) :: dt)) :: u8
    )
    u8_sat(f32)(strength * lf32(32.0f)) |> fun(strength_x32 =>
      input |> map(fun(plane_ => {
        val plane = Image(0, w+2, 0, h+2, plane_)
        letImage(mapImage(
          stencilCollect(Seq((0, -1), (0, 0), (0, 1)), plane),
          blur121(u8)(u16)),
          unsharp_y =>
        letImage(mapImage(
          stencilCollect(Seq((-1, 0), (0, 0), (1, 0)), unsharp_y),
          blur121(u8)(u16)),
          unsharp => {
          letImage(mapImage(zipImage(plane, unsharp), fun(p =>
            (cast(p.`1`) :: i16) - (cast(p.`2`) :: i16)
          )), mask => {
            mapImage(zipImage(plane, mask), fun(p =>
              u8_sat(i16)((cast(p.`1`) :: i16)
                + (p.`2` * (cast(strength_x32) :: i16)) / li16(32))
            )).expr
          })
        }))
      }))
    )
  }))

  // TODO? Halide reference in/out:
  // (h`.`w`.`u16) ->: (3`.`((h - 24) / 32) * 32)`.`((w - 32) / 32) * 32)`.`u8)

  val shift: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    ((h+36)`.`(w+20)`.`u16) ->: (h`.`w`.`i16)
  )(input => input |>
    // shift things inwards to give us enough padding on the
    // boundaries so that we don't need to check bounds. We're going
    // to make a 2560x1920 output image, just like the FCam pipe,
    // so shift by 16, 12.
    map(drop(16 - 6) >> take(w)) >>
    drop(12 - 6) >> take(h) >>
    // We also convert it to be signed, so we can deal with
    // values that fall below 0 during processing.
    map(map(fun(p => cast(p) :: i16)))
  ))

  val camera_pipe: ToBeTyped[Expr] = depFun((h: Nat, w: Nat, hm: Nat, wm: Nat) =>
    fun((2*(h+6)+36)`.`(2*(w+6)+20)`.`u16)(input =>
    fun(hm`.`wm`.`f32)(matrix_3200 =>
    fun(hm`.`wm`.`f32)(matrix_7000 =>
    fun(f32)(color_temp =>
    fun(f32)(gamma =>
    fun(f32)(contrast =>
    fun(int)(blackLevel =>
    fun(int)(whiteLevel =>
    fun(f32 ->: (3`.`(2*h)`.`(2*w)`.`u8))(
      sharpen_strength =>
      input |>
      shift(2*(h+6))(2*(w+6)) >>
      hot_pixel_suppression(2*(h+4))(2*(w+4)) >>
      deinterleave(h+4)(w+4) >>
      // TODO: reorder and store with elevate
      // transpose >> map(transpose) >>
      // mapSeq(mapSeq(mapSeqUnroll(fun(x => x)))) >>
      // map(transpose) >> transpose >>
      demosaic(h+2)(w+2) >>
      // TODO: reorder and store with elevate
      // transpose >> map(transpose) >>
      // split(2) >> mapPar(mapSeqUnroll(
      //  mapSeq(mapSeqUnroll(fun(x => x)))
      // )) >> join >> map(transpose) >> transpose >>
      // --
      map(impl{ w: Nat => map(drop(1) >> take(w)) } >>
        impl{ h: Nat => drop(1) >> take(h) }) >>
      fun(x => color_correct(2*h+2)(2*w+2)(hm)(wm)(x)
        (matrix_3200)(matrix_7000)(color_temp)) >>
      // TODO: reorder and store with elevate
      // transpose >> map(transpose) >>
      // mapPar(mapSeq(mapSeqUnroll(fun(x => x)))) >>
      // map(transpose) >> transpose >>
      // --
      fun(x => apply_curve(2*h+2)(2*w+2)(x)
        (gamma)(contrast)(blackLevel)(whiteLevel)) >>
      // TODO: reorder and store with elevate
      // transpose >> map(transpose) >>
      // mapPar(mapSeq(mapSeqUnroll(fun(x => x)))) >>
      // map(transpose) >> transpose >>
      // --
      fun(x => sharpen(2*h)(2*w)(x)(sharpen_strength)) // >>
      // TODO: reorder and store with elevate
      // transpose >> map(transpose) >>
      // mapPar(mapSeq(mapSeqUnroll(fun(x => x)))) >>
      // map(transpose) >> transpose
      // --
    )))))))))
  )
}
