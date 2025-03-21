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
  def check(module: shine.OpenCL.Module, h: Int, w: Int): Unit = {
    val main = s"""
#include "src/main/scala/apps/harrisCornerDetection2/common.cpp"

int main(int argc, char** argv) {
  Context ctx = createDefaultContext();
  size_t bytes = $h * $w * sizeof(float);
  Buffer input_a = createBuffer(ctx, bytes, HOST_WRITE | HOST_READ | DEVICE_READ);
  Buffer input_b = createBuffer(ctx, bytes, HOST_WRITE | HOST_READ | DEVICE_READ);
  Buffer output = createBuffer(ctx, bytes, HOST_READ | HOST_WRITE | DEVICE_WRITE);

  float* out_gold = (float*) malloc(bytes);

  std::random_device rand_d;
  std::default_random_engine rand_e(rand_d());
  // bigger range results in higher output differences
  std::uniform_real_distribution<float> dist(0, 200);

  float* in_a = (float*) hostBufferSync(ctx, input_a, bytes, HOST_WRITE | HOST_READ);
  float* in_b = (float*) hostBufferSync(ctx, input_b, bytes, HOST_WRITE | HOST_READ);
  for (int y = 0; y < $h; y++) {
    for (int x = 0; x < $w; x++) {
      in_a[y*$w + x] = dist(rand_e);
      in_b[y*$w + x] = dist(rand_e);
    }
  }

  mul_gold(out_gold, $h, $w, in_a, in_b);

  foo_init_run(ctx, output, $h, $w, input_a, input_b);

  ErrorStats errors;
  init_error_stats(&errors);
  float* out = (float*) hostBufferSync(ctx, output, bytes, HOST_READ);
  accumulate_error_stats(&errors, out, out_gold, $h, $w);
  finish_error_stats(&errors, 0.01, 0.0001);

  free(out_gold);
  destroyBuffer(ctx, input_a);
  destroyBuffer(ctx, input_b);
  destroyBuffer(ctx, output);
  destroyContext(ctx);
  return EXIT_SUCCESS;
}
"""
    util.ExecuteOpenCL.using_cpp(main, module, "one_copy")
  }

  def base: ToBeTyped[Expr] =
    depFun((h: Nat, w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32)
    )((a, b) =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        zip(a)(b) |> mapGlobal(fun(ab =>
          zip(ab.`1`)(ab.`2`) |>
          mapSeq(mulT)
        ))
    )))

  val vec: ToBeTyped[Expr] =
    depFun((h: Nat, w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32)
    )((a, b) =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        zip(a)(b) |> mapGlobal(fun(ab =>
          zip(asVectorAligned(vecw)(ab.`1`))(asVectorAligned(vecw)(ab.`2`)) |>
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
            zip(asVectorAligned(vecw)(unzip(ab).`1`))(
              asVectorAligned(vecw)(unzip(ab).`2`)) |>
              mapLocal(0)(mulT)
          ))
        )) >> map(transpose) >> join >> map(join >> asScalar)
    ))))
  }
}
