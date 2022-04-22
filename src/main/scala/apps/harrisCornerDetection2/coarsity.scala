package apps.harrisCornerDetection2

import rise.core._
import rise.core.DSL._
import rise.core.primitives.{id => _, _}
import Type._
import rise.core.types._
import rise.core.types.DataType._
import rise.openCL.DSL._
import shine.OpenCL.{GlobalSize, LocalSize}

object coarsity {
  def base: ToBeTyped[Expr] =
    depFun((h: Nat, w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: f32 ->: (h`.`w`.`f32)
    )((sxx, sxy, syy, kappa) =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        zip(sxx)(zip(sxy)(syy)) |> mapGlobal(fun(s =>
          zip(s._1)(zip(s._2._1)(s._2._2)) |>
            mapSeq(fun(s => {
              val sxx = fst(s)
              val sxy = fst(snd(s))
              val syy = snd(snd(s))
              val det = sxx * syy - sxy * sxy
              val trace = sxx + syy
              det - kappa * trace * trace
            }))
        ))
    )))

  val vec: ToBeTyped[Expr] =
    depFun((h: Nat, w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: f32 ->: (h`.`w`.`f32)
    )((sxx, sxy, syy, kappa) =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        zip(sxx)(zip(sxy)(syy)) |> mapGlobal(fun(s =>
          zip(asVectorAligned(vecw)(s._1))(zip(asVectorAligned(vecw)(s._2._1))(asVectorAligned(vecw)(s._2._2))) |>
          mapSeq(fun(s => {
            val sxx = fst(s)
            val sxy = fst(snd(s))
            val syy = snd(snd(s))
            val det = sxx * syy - sxy * sxy
            val trace = sxx + syy
            det - vectorFromScalar(kappa) * trace * trace
          })) >>
          asScalar
        ))
    )))

  val tile: ToBeTyped[Expr] = {
    val tile_x_in = tile_x
    val tile_y_in = tile_y
    depFun(hFrom(tile_y), (h: Nat) =>
    depFun(wFrom(tile_x), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: f32 ->: (h`.`w`.`f32)
    )((sxx, sxy, syy, kappa) =>
      oclRun(LocalSize((tile_x, tile_y)), GlobalSize((w, h)))(
        zip2D(sxx, zip2D(sxy, syy)) |>
        map(slide(tile_x_in)(tile_x)) |>
        slide(tile_y_in)(tile_y) |>
        map(transpose) |>
        mapWorkGroup(1)(mapWorkGroup(0)(
          mapLocal(1)(fun(s =>
            zip(asVectorAligned(vecw)(unzip(s)._1))(
              zip(asVectorAligned(vecw)(unzip(unzip(s)._2)._1))(
                asVectorAligned(vecw)(unzip(unzip(s)._2)._2))) |>
            mapLocal(0)(fun(s => {
              val sxx = fst(s)
              val sxy = fst(snd(s))
              val syy = snd(snd(s))
              val det = sxx * syy - sxy * sxy
              val trace = sxx + syy
              det - vectorFromScalar(kappa) * trace * trace
            }))
          ))
        )) >> map(transpose) >> join >> map(join >> asScalar)
      ))))
  }

  val tileVec: ToBeTyped[Expr] = {
    val tile_x_in = tile_x
    val tile_y_in = tile_y
    depFun(hFrom(tile_y), (h: Nat) =>
    depFun(wFrom(tile_x), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: f32 ->: (h`.`w`.`f32)
    )((sxx, sxy, syy, kappa) =>
      oclRun(LocalSize((tile_x / vecw, tile_y)), GlobalSize((w / vecw, h)))(
        zip2D(sxx, zip2D(sxy, syy)) |>
        map(slide(tile_x_in)(tile_x)) |>
        slide(tile_y_in)(tile_y) |>
        map(transpose) |>
        mapWorkGroup(1)(mapWorkGroup(0)(
          mapLocal(1)(fun(s =>
            zip(asVectorAligned(vecw)(unzip(s)._1))(
              zip(asVectorAligned(vecw)(unzip(unzip(s)._2)._1))(
                asVectorAligned(vecw)(unzip(unzip(s)._2)._2))) |>
            mapLocal(0)(fun(s => {
              val sxx = fst(s)
              val sxy = fst(snd(s))
              val syy = snd(snd(s))
              val det = sxx * syy - sxy * sxy
              val trace = sxx + syy
              det - vectorFromScalar(kappa) * trace * trace
            }))
          ))
        )) >> map(transpose) >> join >> map(join >> asScalar)
    ))))
  }
}
