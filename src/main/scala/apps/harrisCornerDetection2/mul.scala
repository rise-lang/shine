package apps.harrisCornerDetection2

import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{id => _, _}
import rise.core.types.DataType._
import rise.core.types._
import rise.openCL.DSL._
import shine.OpenCL.{GlobalSize, LocalSize}

object mul {
  def base: ToBeTyped[Expr] =
    depFun((h: Nat, w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32)
    )((a, b) =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        zip(a)(b) |> mapGlobal(fun(ab =>
          zip(ab._1)(ab._2) |>
          mapSeq(mulT)
        ))
    )))

  val vec: ToBeTyped[Expr] =
    depFun((h: Nat, w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32)
    )((a, b) =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        zip(a)(b) |> mapGlobal(fun(ab =>
          zip(asVectorAligned(vecw)(ab._1))(asVectorAligned(vecw)(ab._2)) |>
          mapSeq(mulT) >>
          asScalar
        ))
    )))

  val tile: ToBeTyped[Expr] = {
    val tile_x_in = tile_x
    val tile_y_in = tile_y
    depFun(hFrom(tile_y), (h: Nat) =>
    depFun(wFrom(tile_x), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32)
    )((a, b) =>
      oclRun(LocalSize((tile_x, tile_y)), GlobalSize((w, h)))(
        zip2D(a, b) |>
        map(slide(tile_x_in)(tile_x)) |>
        slide(tile_y_in)(tile_y) |>
        map(transpose) |>
        mapWorkGroup(1)(mapWorkGroup(0)(
          mapLocal(1)(
            mapLocal(0)(mulT)
          )
        )) >> map(transpose) >> join >> map(join)
    ))))
  }

  val tileVec: ToBeTyped[Expr] = {
    val tile_x_in = tile_x
    val tile_y_in = tile_y
    depFun(hFrom(tile_y), (h: Nat) =>
    depFun(wFrom(tile_x), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32)
    )((a, b) =>
      oclRun(LocalSize((tile_x / vecw, tile_y)), GlobalSize((w / vecw, h)))(
        zip2D(a, b) |>
        map(slide(tile_x_in)(tile_x)) |>
        slide(tile_y_in)(tile_y) |>
        map(transpose) |>
        mapWorkGroup(1)(mapWorkGroup(0)(
          mapLocal(1)(fun(ab =>
            zip(asVectorAligned(vecw)(unzip(ab)._1))(
              asVectorAligned(vecw)(unzip(ab)._2)) |>
              mapLocal(0)(mulT)
          ))
        )) >> map(transpose) >> join >> map(join >> asScalar)
    ))))
  }
}
