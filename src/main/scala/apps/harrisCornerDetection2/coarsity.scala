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
  def check(module: shine.OpenCL.Module, h: Int, w: Int, kappa: Float): Unit = {
    val main = s"""
#include "src/main/scala/apps/harrisCornerDetection2/common.cpp"

int main(int argc, char** argv) {
  Context ctx = createDefaultContext();
  size_t bytes = $h * $w * sizeof(float);
  Buffer input_sxx = createBuffer(ctx, bytes, HOST_WRITE | HOST_READ | DEVICE_READ);
  Buffer input_sxy = createBuffer(ctx, bytes, HOST_WRITE | HOST_READ | DEVICE_READ);
  Buffer input_syy = createBuffer(ctx, bytes, HOST_WRITE | HOST_READ | DEVICE_READ);
  Buffer output = createBuffer(ctx, bytes, HOST_READ | HOST_WRITE | DEVICE_WRITE);

  float* out_gold = (float*) malloc(bytes);

  std::random_device rand_d;
  std::default_random_engine rand_e(rand_d());
  // bigger range results in higher output differences
  std::uniform_real_distribution<float> dist(0, 200);

  float* in_sxx = (float*) hostBufferSync(ctx, input_sxx, bytes, HOST_WRITE | HOST_READ);
  float* in_sxy = (float*) hostBufferSync(ctx, input_sxy, bytes, HOST_WRITE | HOST_READ);
  float* in_syy = (float*) hostBufferSync(ctx, input_syy, bytes, HOST_WRITE | HOST_READ);
  for (int y = 0; y < $h; y++) {
    for (int x = 0; x < $w; x++) {
      in_sxx[y*$w + x] = dist(rand_e);
      in_sxy[y*$w + x] = dist(rand_e);
      in_syy[y*$w + x] = dist(rand_e);
    }
  }

  coarsity_gold(out_gold, $h, $w, in_sxx, in_sxy, in_syy, $kappa);

  foo_init_run(ctx, output, $h, $w, input_sxx, input_sxy, input_syy, $kappa);

  ErrorStats errors;
  init_error_stats(&errors);
  float* out = (float*) hostBufferSync(ctx, output, bytes, HOST_READ);
  accumulate_error_stats(&errors, out, out_gold, $h, $w);
  finish_error_stats(&errors, 0.01, 0.0001);

  free(out_gold);
  destroyBuffer(ctx, input_sxx);
  destroyBuffer(ctx, input_sxy);
  destroyBuffer(ctx, input_syy);
  destroyBuffer(ctx, output);
  destroyContext(ctx);
  return EXIT_SUCCESS;
}
"""
    util.ExecuteOpenCL.using_cpp(main, module, "one_copy")
  }

  def base: ToBeTyped[Expr] =
    depFun((h: Nat, w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: f32 ->: (h`.`w`.`f32)
    )((sxx, sxy, syy, kappa) =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        zip(sxx)(zip(sxy)(syy)) |> mapGlobal(fun(s =>
          zip(s.`1`)(zip(s.`2`.`1`)(s.`2`.`2`)) |>
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
          zip(asVectorAligned(vecw)(s.`1`))(zip(asVectorAligned(vecw)(s.`2`.`1`))(asVectorAligned(vecw)(s.`2`.`2`))) |>
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
            zip(asVectorAligned(vecw)(unzip(s).`1`))(
              zip(asVectorAligned(vecw)(unzip(unzip(s).`2`).`1`))(
                asVectorAligned(vecw)(unzip(unzip(s).`2`).`2`))) |>
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
            zip(asVectorAligned(vecw)(unzip(s).`1`))(
              zip(asVectorAligned(vecw)(unzip(unzip(s).`2`).`1`))(
                asVectorAligned(vecw)(unzip(unzip(s).`2`).`2`))) |>
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
