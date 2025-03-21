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

object sobelXYMul {
  def check(module: shine.OpenCL.Module, h: Int, w: Int): Unit = {
    val main = s"""
#include "src/main/scala/apps/harrisCornerDetection2/common.cpp"

int main(int argc, char** argv) {
  Context ctx = createDefaultContext();
  size_t in_bytes = $h * $w * sizeof(float);
  size_t out_h = ${h - 2*bd_h};
  size_t out_w = ${w - 2*bd_w};
  size_t out_bytes = out_h * out_w * sizeof(float);
  Buffer input = createBuffer(ctx, in_bytes, HOST_WRITE | HOST_READ | DEVICE_READ);
  Buffer output = createBuffer(ctx, 3 * out_bytes, HOST_READ | HOST_WRITE | DEVICE_WRITE);

  float* ix_gold = (float*) malloc(out_bytes);
  float* iy_gold = (float*) malloc(out_bytes);
  float* out_ixx_gold = (float*) malloc(out_bytes);
  float* out_ixy_gold = (float*) malloc(out_bytes);
  float* out_iyy_gold = (float*) malloc(out_bytes);

  std::random_device rand_d;
  std::default_random_engine rand_e(rand_d());
  // bigger range results in higher output differences
  std::uniform_real_distribution<float> dist(0, 200);

  float* in = (float*) hostBufferSync(ctx, input, in_bytes, HOST_WRITE | HOST_READ);
  for (int y = 0; y < $h; y++) {
    for (int x = 0; x < $w; x++) {
      in[y*$w + x] = dist(rand_e);
    }
  }

  sobelX_gold(ix_gold, $h, $w, in);
  sobelY_gold(iy_gold, $h, $w, in);
  mul_gold(out_ixx_gold, out_h, out_w, ix_gold, ix_gold);
  mul_gold(out_ixy_gold, out_h, out_w, ix_gold, iy_gold);
  mul_gold(out_iyy_gold, out_h, out_w, iy_gold, iy_gold);

  foo_init_run(ctx, output, $h, $w, input);

  ErrorStats errors;
  init_error_stats(&errors);
  float* out = (float*) hostBufferSync(ctx, output, 3 * out_bytes, HOST_READ);
  accumulate_error_stats(&errors, out, out_ixx_gold, out_h, out_w);
  accumulate_error_stats(&errors, &out[out_h * out_w], out_ixy_gold, out_h, out_w);
  accumulate_error_stats(&errors, &out[2 * out_h * out_w], out_iyy_gold, out_h, out_w);
  finish_error_stats(&errors, 0.01, 0.0001);

  free(ix_gold);
  free(iy_gold);
  free(out_ixx_gold);
  free(out_ixy_gold);
  free(out_iyy_gold);
  destroyBuffer(ctx, input);
  destroyBuffer(ctx, output);
  destroyContext(ctx);
  return EXIT_SUCCESS;
}
"""
    util.ExecuteOpenCL.using_cpp(main, module, "one_copy")
  }

  val base: ToBeTyped[Expr] =
    depFun(hFrom(3), (h: Nat) =>
    depFun(wFrom(12), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (3`.`(h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )(input =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        input |>
        map(drop(bd_w-1) >> take(w - 2*(bd_w-1)) >> slide(3)(1)) >>
        drop(bd_h-1) >> take(h - 2*(bd_h-1)) >> slide(3)(1) >>
        map(transpose) >>
        mapGlobal(mapSeq(fun(nbh =>
          makeArray(2)(sobelXWeights2d)(sobelYWeights2d) |>
          toPrivateFun(mapSeqUnroll(fun(ws => dotSeqU(join(ws))(join(nbh))))) |>
          letf(fun(ixiy => {
            val ix = ixiy `@` lidx(0, 2)
            val iy = ixiy `@` lidx(1, 2)
            makeArray(3)(ix * ix)(ix * iy)(iy * iy) |> mapSeqUnroll(id)
          }))
        ))) >> map(transpose) >> transpose
    ))))

  val lineVec: ToBeTyped[Expr] =
    depFun(hFrom(3), (h: Nat) =>
    depFun(wFrom(12), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (3`.`(h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
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
          toGlobal >>
          slide(3)(1) >>
          mapSeq(
            transpose >> map(shuffle) >>
            zip(makeArray(2)(sobelXWeightsH)(sobelYWeightsH)) >>
            toPrivateFun(mapSeqUnroll(fun(hWsNbh =>
              dotSeqUWV(hWsNbh.`1`)(hWsNbh.`2`)
            ))) >>
            letf(fun(ixiy => {
              val ix = ixiy `@` lidx(0, 2)
              val iy = ixiy `@` lidx(1, 2)
              makeArray(3)(ix * ix)(ix * iy)(iy * iy) |> mapSeqUnroll(id)
            }))
          ) >> transpose >> map(asScalar)
      ) >> transpose
    ))))

  val rotvVec: ToBeTyped[Expr] =
    depFun(hFrom(3), (h: Nat) =>
    depFun(wFrom(12), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (3`.`(h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
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
            toPrivateFun(mapSeqUnroll(fun(hWsNbh =>
              dotSeqUWV(hWsNbh.`1`)(hWsNbh.`2`)
            ))) >>
            letf(fun(ixiy => {
              val ix = ixiy `@` lidx(0, 2)
              val iy = ixiy `@` lidx(1, 2)
              makeArray(3)(ix * ix)(ix * iy)(iy * iy) |> mapSeqUnroll(id)
            }))
          ) >> transpose >> map(asScalar)
      ) >> transpose
    ))))

  val tile: ToBeTyped[Expr] = {
    val tile_x_in = tile_x + 2
    val tile_y_in = tile_y + 2
    depFun(hFrom(tile_y), (h: Nat) =>
    depFun(wFrom(tile_x), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (3`.`(h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
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
            toPrivateFun(mapSeqUnroll(fun(ws => dotSeqU(join(ws))(join(nbh))))) |>
            letf(fun(ixiy => {
              val ix = ixiy `@` lidx(0, 2)
              val iy = ixiy `@` lidx(1, 2)
              makeArray(3)(ix * ix)(ix * iy)(iy * iy) |> mapSeqUnroll(id)
            }))
          )))
        )) >> map(transpose) >> join >> map(join) >>
        map(transpose) >> transpose
    ))))
  }

  val tileVec: ToBeTyped[Expr] = {
    val tile_x_in = tile_x + 2*vecw
    val tile_y_in = tile_y + 2
    depFun(hFrom(tile_y), (h: Nat) =>
    depFun(wFrom(tile_x), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (3`.`(h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
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
            toPrivateFun(mapSeqUnroll(fun(ws => dotSeqUWV(join(ws))(join(map(shuffle)(nbh)))))) |>
            letf(fun(ixiy => {
              val ix = ixiy `@` lidx(0, 2)
              val iy = ixiy `@` lidx(1, 2)
              makeArray(3)(ix * ix)(ix * iy)(iy * iy) |> mapSeqUnroll(id)
            }))
          )))
        )) >> map(transpose) >> join >> map(join) >>
        map(transpose >> map(asScalar)) >> transpose
    ))))
  }
}
