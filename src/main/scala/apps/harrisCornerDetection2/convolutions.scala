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
  def check(prelude: String, module: shine.OpenCL.Module, h: Int, w: Int): Unit = {
    val main = s"""
${prelude}

int main(int argc, char** argv) {
  Context ctx = createDefaultContext();
  size_t in_bytes = $h * $w * sizeof(float);
  size_t out_bytes = ${h - 2*bd_h} * ${w - 2*bd_w} * sizeof(float);
  Buffer input = createBuffer(ctx, in_bytes, HOST_WRITE | HOST_READ | DEVICE_READ);
  Buffer output = createBuffer(ctx, out_bytes, HOST_READ | HOST_WRITE | DEVICE_WRITE);

  float* out_gold = (float*) malloc(out_bytes);

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

  gold(out_gold, in);

  generated(ctx, output, input);

  ErrorStats errors;
  init_error_stats(&errors);
  float* out = (float*) hostBufferSync(ctx, output, out_bytes, HOST_READ);
  accumulate_error_stats(&errors, out, out_gold, ${h - 2*bd_h}, ${w - 2*bd_w});
  finish_error_stats(&errors, 0.01, 0.0001);

  free(out_gold);
  destroyBuffer(ctx, input);
  destroyBuffer(ctx, output);
  destroyContext(ctx);
  return EXIT_SUCCESS;
}
"""
    util.ExecuteOpenCL.using_cpp(main, module, "one_copy")
  }

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
          toGlobal >>
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
