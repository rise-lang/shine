package apps.harrisCornerDetection2

import rise.core._
import rise.core.DSL._
import rise.core.primitives.{id => _, _}
import Type._
import rise.core.types._
import rise.core.types.DataType._
import rise.openCL.DSL._
import rise.openCL.primitives.oclRotateValues
import shine.OpenCL.{GlobalSize, LocalSize}

object binomialCoarsity {
  def check(module: shine.OpenCL.Module, h: Int, w: Int, kappa: Float): Unit = {
    val main = s"""
#include "src/main/scala/apps/harrisCornerDetection2/common.cpp"

int main(int argc, char** argv) {
  Context ctx = createDefaultContext();
  size_t in_bytes = $h * $w * sizeof(float);
  size_t out_bytes = ${h - 2*bd_h} * ${w - 2*bd_w} * sizeof(float);
  Buffer input_ixx = createBuffer(ctx, in_bytes, HOST_WRITE | HOST_READ | DEVICE_READ);
  Buffer input_ixy = createBuffer(ctx, in_bytes, HOST_WRITE | HOST_READ | DEVICE_READ);
  Buffer input_iyy = createBuffer(ctx, in_bytes, HOST_WRITE | HOST_READ | DEVICE_READ);
  Buffer output = createBuffer(ctx, out_bytes, HOST_READ | HOST_WRITE | DEVICE_WRITE);

  float* sxx_gold = (float*) malloc(out_bytes);
  float* sxy_gold = (float*) malloc(out_bytes);
  float* syy_gold = (float*) malloc(out_bytes);
  float* out_gold = (float*) malloc(out_bytes);

  std::random_device rand_d;
  std::default_random_engine rand_e(rand_d());
  // bigger range results in higher output differences
  std::uniform_real_distribution<float> dist(0, 200);

  float* in_ixx = (float*) hostBufferSync(ctx, input_ixx, in_bytes, HOST_WRITE | HOST_READ);
  float* in_ixy = (float*) hostBufferSync(ctx, input_ixy, in_bytes, HOST_WRITE | HOST_READ);
  float* in_iyy = (float*) hostBufferSync(ctx, input_iyy, in_bytes, HOST_WRITE | HOST_READ);
  for (int y = 0; y < $h; y++) {
    for (int x = 0; x < $w; x++) {
      in_ixx[y*$w + x] = dist(rand_e);
      in_ixy[y*$w + x] = dist(rand_e);
      in_iyy[y*$w + x] = dist(rand_e);
    }
  }

  binomial_gold(sxx_gold, $h, $w, in_ixx);
  binomial_gold(sxy_gold, $h, $w, in_ixy);
  binomial_gold(syy_gold, $h, $w, in_iyy);
  coarsity_gold(out_gold, ${h - 2*bd_h}, ${w - 2*bd_w}, sxx_gold, sxy_gold, syy_gold, $kappa);

  foo_init_run(ctx, output, $h, $w, input_ixx, input_ixy, input_iyy, $kappa);

  ErrorStats errors;
  init_error_stats(&errors);
  float* out = (float*) hostBufferSync(ctx, output, out_bytes, HOST_READ);
  accumulate_error_stats(&errors, out, out_gold, ${h - 2*bd_h}, ${w - 2*bd_w});
  finish_error_stats(&errors, 0.05, 0.0001);

  free(sxx_gold);
  free(sxy_gold);
  free(syy_gold);
  free(out_gold);
  destroyBuffer(ctx, input_ixx);
  destroyBuffer(ctx, input_ixy);
  destroyBuffer(ctx, input_iyy);
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
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: f32
        ->: ((h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )((sxx, sxy, syy, kappa) =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        makeArray(3)(sxx)(sxy)(syy) |>
        transpose >> map(transpose) >>
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
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: f32
        ->: ((h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )((sxx, sxy, syy, kappa) =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        makeArray(3)(sxx)(sxy)(syy) |>
        map(
          map(drop(bd_w-vecw) >> take(w - 2*(bd_w-vecw))) >>
          drop(bd_h-1) >> take(h - 2*(bd_h-1)) >>
          map(asVectorAligned(vecw))
        ) >>
        transpose >> map(transpose) >>
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
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32)
        ->: f32 ->: ((h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )((sxx, sxy, syy, kappa) =>
      oclRun(LocalSize(1), GlobalSize(num_threads))(
        makeArray(3)(sxx)(sxy)(syy) |>
        map(
          map(drop(bd_w-vecw) >> take(w - 2*(bd_w-vecw))) >>
          drop(bd_h-1) >> take(h - 2*(bd_h-1)) >>
          map(asVectorAligned(vecw))
        ) >>
        transpose >> map(transpose) >>
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
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: f32
        ->: ((h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )((sxx, sxy, syy, kappa) =>
      oclRun(LocalSize((tile_x, tile_y)), GlobalSize((w - 2*bd_w, h - 2*bd_h)))(
        makeArray(3)(sxx)(sxy)(syy) |>
        transpose >> map(transpose) >>
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
    val tile_x_in = tile_x + 2*vecw
    val tile_y_in = tile_y + 2
    depFun(hFrom(tile_y), (h: Nat) =>
    depFun(wFrom(tile_x), (w: Nat) => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: f32
        ->: ((h - 2*bd_h)`.`(w - 2*bd_w)`.`f32)
    )((sxx, sxy, syy, kappa) =>
      oclRun(LocalSize((tile_x / vecw, tile_y)), GlobalSize(((w - 2*bd_w) / vecw, h - 2*bd_h)))(
        makeArray(3)(sxx)(sxy)(syy) |>
        map(
          map(drop(bd_w-vecw) >> take(w - 2*(bd_w-vecw)) >> slide(tile_x_in)(tile_x)) >>
          drop(bd_h-1) >> take(h - 2*(bd_h-1)) >> slide(tile_y_in)(tile_y) >>
          map(transpose) >>
          map(map(
            map(asVectorAligned(vecw) >> slide(3)(1)) >> slide(3)(1) >> map(transpose)
          ))
        ) >>
        transpose >> map(transpose >> map(transpose >> map(transpose))) >>
        mapWorkGroup(1)(mapWorkGroup(0)(
          mapLocal(1)(mapLocal(0)(
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
