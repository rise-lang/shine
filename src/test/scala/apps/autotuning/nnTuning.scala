package apps.autotuning

import rise.autotune
import rise.autotune.{Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.types._
import rise.core.primitives._
import rise.openCL.DSL._
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen

class nnTuning extends test_util.Tests {

  // define nn
  private val distance = foreignFun("distance_",
    Seq("loc", "lat", "lng"),
    "{ return sqrt((lat - loc._fst) * (lat - loc._fst) + (lng - loc._snd) * (lng -  loc._snd)); }",
    (f32 x f32) ->: f32 ->: f32 ->: f32
  )

  // FIXME: could not find original Lift expression, this is made up
  val nnHighLevel: ToBeTyped[Expr] = depFun((n: Nat) => fun(
    (n`.`(f32 x f32)) ->: f32 ->: f32 ->: (n`.`f32)
  )((locations, lat, lng) =>
    locations |> map(fun(loc => distance(loc)(lat)(lng)))
  ))

  val nnOcl: ToBeTyped[Expr] = depFun((n: Nat) => fun(
    (n `.` (f32 x f32)) ->: f32 ->: f32 ->: (n `.` f32)
  )((locations, lat, lng) =>
    locations |> mapGlobal(fun(loc => distance(loc)(lat)(lng)))
  ))

  val nnTuning: Expr =
    tuningParam("n", (n: Nat)
    => fun(
        (n `.` (f32 x f32)) ->: f32 ->: f32 ->: (n `.` f32)
      )((locations, lat, lng) =>
        locations |> mapGlobal(fun(loc => distance(loc)(lat)(lng)))
      ))

  val nn:Expr =
  tuningParam("ls0", (ls0: Nat) =>
    tuningParam("gs0", (gs0: Nat) =>
      wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(nnTuning)))

  // define main

  val main: (Int, Int) => String = (N, K) => {
    s"""
        const int N = ${N};
        const int K = ${K};
        int main(int argc, char** argv) {
          srand(time(NULL));
          Context ctx = createDefaultContext();
          Buffer input = createBuffer(ctx, 2 * N * sizeof(float), HOST_WRITE | DEVICE_READ);
          Buffer output = createBuffer(ctx, K * sizeof(float), HOST_READ | DEVICE_WRITE);

          float* in = hostBufferSync(ctx, input, 2 * N * sizeof(float), HOST_WRITE);
          for (int i = 0; i < 2 * N ; i++) {
//            in[i] = (float)(rand());
              in[i] = i;
          }

          float lat = 2.0f;
          float lng = 2.0f;

//          foo_init_run(ctx, output, K, input, lat, lng);
          foo_init_run(ctx, output, input, lat, lng);

          float* out = hostBufferSync(ctx, output, K * sizeof(float), HOST_READ);

//          for (int i = 0; i < K ; i++) {
//             printf(" %f ", out[i]);
//          }
//          printf("\\n");

    //    todo add error checking

          destroyBuffer(ctx, input);
          destroyBuffer(ctx, output);
          destroyContext(ctx);
          return EXIT_SUCCESS;
        }
        """
  }

  // define nn with tuning parameters
  // tune gs ls of nn

  // apply rewrites to nn
  // optimize -> introducing new tuning parameters

  // test to tune nn
  test("ocl nn"){

    val nn = wrapOclRun(LocalSize(1, 1, 1), GlobalSize(4, 1, 1))(nnOcl)
    println("nn: \n " + nn)

    val codeHosted = gen.opencl.hosted.fromExpr(nn)
    println("codeHosted: \n" + codeHosted)
    println("codeHosted: \n" + gen.opencl.hosted.asString(codeHosted))

    val program = shine.OpenCL.Module.translateToString(codeHosted) + main(8,4)

    println("program: " + program )
  }

  test("codegen nn"){
    println("nn: \n" + nn)

    val params = Map(
      TuningParameter("n") -> (4: Nat),
      TuningParameter("ls0") -> (1: Nat),
      TuningParameter("gs0") -> (8: Nat)
    )

    val nn2 = rise.core.substitute.natsInExpr(params, nn)
    println("nn2: \n" + nn2)

    val codeHosted = gen.opencl.hosted.fromExpr(nn2)
    println("codeHosted: \n" + codeHosted)
    println("codeHosted: \n" + gen.opencl.hosted.asString(codeHosted))

    val program = shine.OpenCL.Module.translateToString(codeHosted) + main(8,4)

    println("program: " + program )

  }

  test("execute nn"){

    println("nn: \n" + nn)

    val params = Map(
      TuningParameter("n") -> (4: Nat),
      TuningParameter("ls0") -> (1: Nat),
      TuningParameter("gs0") -> (8: Nat)
    )

    val nn2 = rise.core.substitute.natsInExpr(params, nn)
    println("nn2: \n" + nn2)

    val result = autotune.execution.execute(nn2, main(8,4), Timeouts(5000, 5000, 5000), 10, 100)

    println("result: " + result)

  }

  test("search nn"){

    // todo add constraints to config file
    val tuner = Tuner(main(1024,512), 100, "nn", "autotuning/nn", Timeouts(10000, 10000, 10000), 10, 100, Some("/home/jo/development/rise-lang/shine/autotuning/config/nn_1024.json"), false)

    val tuningResult = autotune.search(tuner)(nn)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }
}
