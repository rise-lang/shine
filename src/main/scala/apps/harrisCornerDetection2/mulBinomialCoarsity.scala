package apps.harrisCornerDetection2

import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{id => _, _}
import rise.core.types.DataType._
import rise.core.types._
import rise.openCL.DSL._
import rise.openCL.primitives.oclRotateValues
import shine.OpenCL.{GlobalSize, LocalSize}

object mulBinomialCoarsity {
  val base: ToBeTyped[Expr] =
    depFun(hFrom(3), (h: Nat) =>
    depFun(wFrom(12), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: f32
        ->: ((h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )((ix, iy, kappa) =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        makeArray(2)(ix)(iy) |>
        transpose >> map(transpose) >>
        map(map(fun(ixiy => {
          val ix = ixiy `@` lidx(0, 2)
          val iy = ixiy `@` lidx(1, 2)
          makeArray(3)(ix * ix)(ix * iy)(iy * iy)
        }))) >>
        map(drop(bd_w-1) >> take(w - 2*(bd_w-1)) >> slide(3)(1)) >>
        drop(bd_h-1) >> take(h - 2*(bd_h-1)) >> slide(3)(1) >>
        map(transpose) >>
        mapGlobal(mapSeq(
          map(transpose) >> transpose >>
          toPrivateFun(mapSeqUnroll(fun(nbh =>
            dotSeqU(join(binomialWeights2d))(join(nbh))
          ))) >>
          letf(fun(s => {
            val sxx = s `@` lidx(0, 3)
            val sxy = s `@` lidx(1, 3)
            val syy = s `@` lidx(2, 3)
            val det = sxx * syy - sxy * sxy
            val trace = sxx + syy
            det - kappa * trace * trace
          }))
        ))
    ))))

  val lineVec: ToBeTyped[Expr] =
    depFun(hFrom(3), (h: Nat) =>
    depFun(wFrom(12), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: f32
        ->: ((h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )((ix, iy, kappa) =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        makeArray(2)(ix)(iy) |>
        map(
          map(drop(bd_w-vecw) >> take(w - 2*(bd_w-vecw))) >>
          drop(bd_h-1) >> take(h - 2*(bd_h-1)) >>
          map(asVectorAligned(vecw))
        ) >>
        transpose >> map(transpose) >>
        map(map(fun(ixiy => {
          val ix = ixiy `@` lidx(0, 2)
          val iy = ixiy `@` lidx(1, 2)
          makeArray(3)(ix * ix)(ix * iy)(iy * iy)
        }))) >>
        slide(3)(1) >> mapGlobal(
          transpose >> map(transpose) >>
          mapSeq(mapSeqUnroll(dotSeqUWV(binomialWeightsV))) >>
          toGlobal >>
          slide(3)(1) >>
          mapSeq(
            transpose >> map(shuffle) >>
            toPrivateFun(mapSeqUnroll(dotSeqUWV(binomialWeightsH))) >>
            letf(fun(s => {
              val sxx = s `@` lidx(0, 3)
              val sxy = s `@` lidx(1, 3)
              val syy = s `@` lidx(2, 3)
              val det = sxx * syy - sxy * sxy
              val trace = sxx + syy
              det - vectorFromScalar(kappa) * trace * trace
            }))
          ) >> asScalar
      )
    ))))

  val rotvVec: ToBeTyped[Expr] =
    depFun(hFrom(3), (h: Nat) =>
    depFun(wFrom(12), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: f32
        ->: ((h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )((ix, iy, kappa) =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        makeArray(2)(ix)(iy) |>
        map(
          map(drop(bd_w-vecw) >> take(w - 2*(bd_w-vecw))) >>
          drop(bd_h-1) >> take(h - 2*(bd_h-1)) >>
          map(asVectorAligned(vecw))
        ) >>
        transpose >> map(transpose) >>
        map(map(fun(ixiy => {
          val ix = ixiy `@` lidx(0, 2)
          val iy = ixiy `@` lidx(1, 2)
          makeArray(3)(ix * ix)(ix * iy)(iy * iy)
        }))) >>
        slide(3)(1) >> mapGlobal(
          transpose >> map(transpose) >>
          map(map(dotSeqUWV(binomialWeightsV))) >>
          oclRotateValues(AddressSpace.Private)(3)(mapSeqUnroll(id)) >> iterateStream(
            transpose >> map(shuffle) >>
            toPrivateFun(mapSeqUnroll(dotSeqUWV(binomialWeightsH))) >>
            letf(fun(s => {
              val sxx = s `@` lidx(0, 3)
              val sxy = s `@` lidx(1, 3)
              val syy = s `@` lidx(2, 3)
              val det = sxx * syy - sxy * sxy
              val trace = sxx + syy
              det - vectorFromScalar(kappa) * trace * trace
            }))
          ) >> asScalar
      )
    ))))

  val tile: ToBeTyped[Expr] = {
    val tile_x_in = tile_x + 2
    val tile_y_in = tile_y + 2
    depFun(hFrom(tile_y), (h: Nat) =>
    depFun(wFrom(tile_x), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: f32
        ->: ((h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )((ix, iy, kappa) =>
      oclRun(LocalSize((tile_x, tile_y)), GlobalSize((w - 2*bd_w, h - 2*bd_h)))(
        makeArray(2)(ix)(iy) |>
        transpose >> map(transpose) >>
        map(map(fun(ixiy => {
          val ix = ixiy `@` lidx(0, 2)
          val iy = ixiy `@` lidx(1, 2)
          makeArray(3)(ix * ix)(ix * iy)(iy * iy)
        }))) >>
        map(drop(bd_w-1) >> take(w - 2*(bd_w-1)) >> slide(tile_x_in)(tile_x)) >>
        drop(bd_h-1) >> take(h - 2*(bd_h-1)) >> slide(tile_y_in)(tile_y) >>
        map(transpose) >>
        map(map(
          map(slide(3)(1)) >> slide(3)(1) >> map(transpose)
        )) >>
        mapWorkGroup(1)(mapWorkGroup(0)(
          mapLocal(1)(mapLocal(0)(
            map(transpose) >> transpose >>
            toPrivateFun(mapSeqUnroll(fun(nbh =>
              dotSeqU(join(binomialWeights2d))(join(nbh))
            ))) >>
            letf(fun(s => {
              val sxx = s `@` lidx(0, 3)
              val sxy = s `@` lidx(1, 3)
              val syy = s `@` lidx(2, 3)
              val det = sxx * syy - sxy * sxy
              val trace = sxx + syy
              det - kappa * trace * trace
            }))
          ))
        )) >> map(transpose) >> join >> map(join)
    ))))
  }

  val tileVec: ToBeTyped[Expr] = {
    val tile_vx = tile_x / vecw
    val tile_vx_in = tile_vx + 2
    val tile_y_in = tile_y + 2
    depFun(hFrom(tile_y), (h: Nat) =>
    depFun(wFrom(tile_x), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: f32
        ->: ((h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )((ix, iy, kappa) =>
      oclRun(LocalSize((tile_x / vecw, tile_y)), GlobalSize(((w - 2*bd_w) / vecw, h - 2*bd_h)))(
        makeArray(2)(ix)(iy) |>
        map(
          map(drop(bd_w-vecw) >> take(w - 2*(bd_w-vecw)) >> asVectorAligned(vecw)) >>
          drop(bd_h-1) >> take(h - 2*(bd_h-1))
        ) >>
        transpose >> map(transpose) >>
        map(map(fun(ixiy => {
          val ix = ixiy `@` lidx(0, 2)
          val iy = ixiy `@` lidx(1, 2)
          makeArray(3)(ix * ix)(ix * iy)(iy * iy)
        }))) >>
        map(slide(tile_vx_in)(tile_vx)) >>
        slide(tile_y_in)(tile_y) >>
        map(transpose) >>
        map(map(
          map(slide(3)(1)) >> slide(3)(1) >> map(transpose)
        )) >>
        mapWorkGroup(1)(mapWorkGroup(0)(
          mapLocal(1)(mapLocal(0)(
            map(transpose) >> transpose >>
            toPrivateFun(mapSeqUnroll(fun(nbh =>
              dotSeqUWV(join(binomialWeights2d))(join(map(shuffle)(nbh)))
            ))) >>
            letf(fun(s => {
              val sxx = s `@` lidx(0, 3)
              val sxy = s `@` lidx(1, 3)
              val syy = s `@` lidx(2, 3)
              val det = sxx * syy - sxy * sxy
              val trace = sxx + syy
              det - vectorFromScalar(kappa) * trace * trace
            }))
          ))
        )) >> map(transpose) >> join >> map(join >> asScalar)
    ))))
  }
}
