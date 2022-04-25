package apps.harrisCornerDetection2

import rise.core._
import rise.core.DSL._
import rise.core.primitives.{id => _, _}
import Type._
import rise.core.types._
import rise.core.types.DataType._
import rise.core.DSL.Type._
import rise.openCL.DSL._
import rise.openCL.primitives.oclRotateValues
import shine.OpenCL.{GlobalSize, LocalSize}

object convolutions {
  def base(weights2d: ToBeTyped[Expr]): ToBeTyped[Expr] =
    depFun(hFrom(3), (h: Nat) =>
    depFun(wFrom(12), (w: Nat) => fun(
      (h`.`w`.`f32) ->: ((h - 2*bd_h)`.`(w-2*bd_w)`.`f32)
    )(input =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        input |>
        map(drop(bd_w-1) >> take(w - 2*(bd_w-1)) >> slide(3)(1)) >>
        drop(bd_h-1) >> take(h - 2*(bd_h-1)) >> slide(3)(1) >>
        map(transpose) >>
        mapGlobal(mapSeq(fun(nbh =>
          dotSeqU(join(weights2d))(join(nbh))
        )))
    ))))

  def lineVec(weightsV: ToBeTyped[Expr], weightsH: ToBeTyped[Expr]): ToBeTyped[Expr] =
    depFun(hFrom(3), (h: Nat) =>
    depFun(wFrom(12), (w: Nat) => fun(
      (h`.`w`.`f32) ->: ((h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )(input =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        input |>
        map(drop(bd_w-vecw) >> take(w - 2*(bd_w-vecw))) >>
        drop(bd_h-1) >> take(h - 2*(bd_h-1)) >>
        map(asVectorAligned(vecw)) >> slide(3)(1) >> mapGlobal(
          transpose >>
          mapSeq(dotSeqUWV(weightsV)) >>
          // FIXME: toGlobal? + should not need to avoid vector
          impl { (t: DataType) => (asScalar >> toLocal >> asVectorAligned(vecw)) :: (t ->: t) } >>
          // toLocal >>
          slide(3)(1) >>
          mapSeq(shuffle >> dotSeqUWV(weightsH)) >>
          asScalar
        )
    ))))

  def rotvVec(weightsV: ToBeTyped[Expr], weightsH: ToBeTyped[Expr]): ToBeTyped[Expr] =
    depFun(hFrom(3), (h: Nat) =>
    depFun(wFrom(12), (w: Nat) => fun(
      (h`.`w`.`f32) ->: ((h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )(input =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        input |>
        map(drop(bd_w-vecw) >> take(w - 2*(bd_w-vecw))) >>
        drop(bd_h-1) >> take(h - 2*(bd_h-1)) >>
        map(asVectorAligned(vecw)) >> slide(3)(1) >> mapGlobal(
          transpose >>
          map(dotSeqUWV(weightsV)) >>
          oclRotateValues(AddressSpace.Private)(3)(id) >> iterateStream(
            shuffle >> dotSeqUWV(weightsH)
          ) >>
          asScalar
        )
    ))))

  def tile(weights2d: ToBeTyped[Expr]): ToBeTyped[Expr] = {
    val tile_x_in = tile_x + 2
    val tile_y_in = tile_y + 2
    depFun(hFrom(tile_y), (h: Nat) =>
    depFun(wFrom(tile_x), (w: Nat) => fun(
      (h`.`w`.`f32) ->: ((h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )(input =>
      oclRun(LocalSize((tile_x, tile_y)), GlobalSize(((w - 2*bd_w), h - 2*bd_h)))(
        input |>
        map(drop(bd_w-1) >> take(w - (2*bd_w-2)) >> slide(tile_x_in)(tile_x)) >>
          drop(bd_h-1) >> take(h - (2*bd_h-2)) >> slide(tile_y_in)(tile_y) >>
          map(transpose) >>
          map(map(
            map(slide(3)(1)) >> slide(3)(1) >> map(transpose)
          )) >>
          mapWorkGroup(1)(mapWorkGroup(0)(
            mapLocal(1)(mapLocal(0)(fun(nbh =>
              dotSeqU(join(weights2d))(join(nbh))
            )))
          )) >> map(transpose) >> join >> map(join)
    ))))
  }

  def tileVec(weights2d: ToBeTyped[Expr]): ToBeTyped[Expr] = {
    val tile_x_in = tile_x + 2*vecw
    val tile_y_in = tile_y + 2
    depFun(hFrom(tile_y), (h: Nat) =>
    depFun(wFrom(tile_x), (w: Nat) => fun(
      (h`.`w`.`f32) ->: ((h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )(input =>
      oclRun(LocalSize((tile_x / vecw, tile_y)), GlobalSize(((w - 2*bd_w) / vecw, h - 2*bd_h)))(
        input |>
        map(drop(bd_w-vecw) >> take(w - 2*(bd_w-vecw)) >> slide(tile_x_in)(tile_x)) >>
        drop(bd_h-1) >> take(h - 2*(bd_h-1)) >> slide(tile_y_in)(tile_y) >>
        map(transpose) >>
        map(map(
          map(asVectorAligned(vecw) >> slide(3)(1)) >> slide(3)(1) >> map(transpose)
        )) >>
        mapWorkGroup(1)(mapWorkGroup(0)(
          mapLocal(1)(mapLocal(0)(fun(nbh =>
            dotSeqUWV(join(weights2d))(join(map(shuffle)(nbh)))
          )))
        )) >> map(transpose) >> join >> map(join >> asScalar)
      ))
    ))
  }
}
