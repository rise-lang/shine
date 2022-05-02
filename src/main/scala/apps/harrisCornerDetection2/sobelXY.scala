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

object sobelXY {
  val base: ToBeTyped[Expr] =
    depFun(hFrom(3), (h: Nat) =>
    depFun(wFrom(12), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (2`.`(h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )(input =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        input |>
        map(drop(bd_w-1) >> take(w - 2*(bd_w-1)) >> slide(3)(1)) >>
        drop(bd_h-1) >> take(h - 2*(bd_h-1)) >> slide(3)(1) >>
        map(transpose) >>
        mapGlobal(mapSeq(fun(nbh =>
          makeArray(2)(sobelXWeights2d)(sobelYWeights2d) |>
          mapSeqUnroll(fun(ws => dotSeqU(join(ws))(join(nbh))))
        ))) >> map(transpose) >> transpose
    ))))

  val lineVec: ToBeTyped[Expr] =
    depFun(hFrom(3), (h: Nat) =>
    depFun(wFrom(12), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (2`.`(h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )(input =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        input |>
        map(drop(bd_w-vecw) >> take(w - 2*(bd_w-vecw))) >>
        drop(bd_h-1) >> take(h - 2*(bd_h-1)) >>
        map(asVectorAligned(vecw)) >> slide(3)(1) >> mapGlobal(
          transpose >>
          mapSeq(fun(vNbh =>
            makeArray(2)(sobelXWeightsV)(sobelYWeightsV) |>
            mapSeqUnroll(fun(ws => dotSeqUWV(ws)(vNbh)))
          )) >>
          // FIXME: should not need to avoid vector
          impl { (t: DataType) => (map(asScalar) >> toGlobal >> map(asVectorAligned(vecw))) :: (t ->: t) } >>
          slide(3)(1) >>
          mapSeq(
            transpose >> map(shuffle) >>
            zip(makeArray(2)(sobelXWeightsH)(sobelYWeightsH)) >>
            mapSeqUnroll(fun(hWsNbh =>
              dotSeqUWV(hWsNbh._1)(hWsNbh._2)
            ))
          ) >> transpose >> map(asScalar)
      ) >> transpose
    ))))

  val rotvVec: ToBeTyped[Expr] =
    depFun(hFrom(3), (h: Nat) =>
    depFun(wFrom(12), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (2`.`(h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )(input =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        input |>
        map(drop(bd_w-vecw) >> take(w - 2*(bd_w-vecw))) >>
        drop(bd_h-1) >> take(h - 2*(bd_h-1)) >>
        map(asVectorAligned(vecw)) >> slide(3)(1) >> mapGlobal(
        transpose >>
          map(fun(vNbh =>
            makeArray(2)(sobelXWeightsV)(sobelYWeightsV) |>
            map(fun(ws => dotSeqUWV(ws)(vNbh)))
          )) >>
          oclRotateValues(AddressSpace.Private)(3)(mapSeqUnroll(id)) >> iterateStream(
            transpose >> map(shuffle) >>
            zip(makeArray(2)(sobelXWeightsH)(sobelYWeightsH)) >>
            mapSeqUnroll(fun(hWsNbh => dotSeqUWV(hWsNbh._1)(hWsNbh._2)))
          ) >> transpose >> map(asScalar)
      ) >> transpose
    ))))

  val tile: ToBeTyped[Expr] = {
    val tile_x_in = tile_x + 2
    val tile_y_in = tile_y + 2
    depFun(hFrom(tile_y), (h: Nat) =>
    depFun(wFrom(tile_x), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (2`.`(h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )(input =>
      oclRun(LocalSize((tile_x, tile_y)), GlobalSize(((w - 2*bd_w), h - 2*bd_h)))(
        input |>
        map(drop(bd_w-1) >> take(w - 2*(bd_w-1)) >> slide(tile_x_in)(tile_x)) >>
        drop(bd_h-1) >> take(h - 2*(bd_h-1)) >> slide(tile_y_in)(tile_y) >>
        map(transpose) >>
        map(map(
          map(slide(3)(1)) >> slide(3)(1) >> map(transpose)
        )) >>
        mapWorkGroup(1)(mapWorkGroup(0)(
          mapLocal(1)(mapLocal(0)(fun(nbh =>
            makeArray(2)(sobelXWeights2d)(sobelYWeights2d) |>
            mapSeqUnroll(fun(ws => dotSeqU(join(ws))(join(nbh))))
          ))) // ty.tx.2.f
        )) >> map(transpose) >> join >> map(join) >>
        map(transpose) >> transpose
    ))))
  }

  val tileVec: ToBeTyped[Expr] = {
    val tile_x_in = tile_x + 2*vecw
    val tile_y_in = tile_y + 2
    depFun(hFrom(tile_y), (h: Nat) =>
    depFun(wFrom(tile_x), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (2`.`(h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
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
            makeArray(2)(sobelXWeights2d)(sobelYWeights2d) |>
            mapSeqUnroll(fun(ws => dotSeqUWV(join(ws))(join(map(shuffle)(nbh)))))
          )))
        )) >> map(transpose) >> join >> map(join) >>
        map(transpose >> map(asScalar)) >> transpose
    ))))
  }
}
