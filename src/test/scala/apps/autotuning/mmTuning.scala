package apps.autotuning

import apps.mm.mmNVIDIAWithParams
import rise.core._
import rise.core.types._
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.autotune
import shine.OpenCL.{GlobalSize, LocalSize}
import rise.autotune._

class mmTuning extends test_util.Tests {

  val mmTuning: ToBeTyped[Expr] =
    tuningParam("n", (n: Nat) =>
      tuningParam("m", (m: Nat) =>
        tuningParam("o", (o: Nat) =>
          tuningParam("v3", (v3: Nat) =>
            tuningParam("v4", (v4: Nat) =>
              tuningParam("v5", (v5: Nat) =>
                tuningParam("v6", (v6: Nat) =>
                  tuningParam("v7", (v7: Nat) =>
                    tuningParam("v8", (v8: Nat) =>
                      mmNVIDIAWithParams(m, n, o, v3, v4, v5, v6, v7, v8)
                    )))))))))

  val main: (Int, Int, Int) => String = (N,M,O) => {
    s"""
        const int N = ${N};
        const int M = ${M};
        const int O = ${O};
        int main(int argc, char** argv) {
          srand(time(NULL));
          Context ctx = createDefaultContext();
          Buffer inputA = createBuffer(ctx, N * M * sizeof(float), HOST_WRITE | DEVICE_READ);
          Buffer inputB = createBuffer(ctx, M * O * sizeof(float), HOST_WRITE | DEVICE_READ);
          Buffer outputC = createBuffer(ctx, N * O *  sizeof(float), HOST_READ | DEVICE_WRITE);

          float* inA = hostBufferSync(ctx, inputA, N * M * sizeof(float), HOST_WRITE);
          for (int i = 0; i < N * M ; i++) {
            inA[i] = (float)(rand());
          }

          float* inB = hostBufferSync(ctx, inputB, M * O * sizeof(float), HOST_WRITE);
          for (int i = 0; i < M * O; i++) {
            inB[i] = (float)(rand());
          }

          foo_init_run(ctx, outputC, inputA, inputB);

          float* out = hostBufferSync(ctx, outputC, N * O * sizeof(float), HOST_READ);

    //    todo add error checking

          destroyBuffer(ctx, inputA);
          destroyBuffer(ctx, inputB);
          destroyBuffer(ctx, outputC);
          destroyContext(ctx);
          return EXIT_SUCCESS;
        }
        """
  }

  test("mm example config"){
    val mm: Expr =
      tuningParam("ls0", (ls0: Nat) => tuningParam("ls1", (ls1: Nat) =>
        tuningParam("gs0", (gs0: Nat) => tuningParam("gs1", (gs1: Nat) =>
          wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning)))))

    val params0 = Map(
      TuningParameter("n") -> (1024: Nat),
      TuningParameter("m") -> (1024: Nat),
      TuningParameter("o") -> (1024: Nat),
      TuningParameter("ls0") -> (16: Nat),
      TuningParameter("ls1") -> (16: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (256: Nat),
      TuningParameter("v3") -> (4: Nat),
      TuningParameter("v4") -> (1: Nat),
      TuningParameter("v5") -> (4: Nat),
      TuningParameter("v6") -> (64: Nat),
      TuningParameter("v7") -> (256: Nat),
      TuningParameter("v8") -> (32: Nat)
    )

    val mm0 = rise.core.substitute.natsInExpr(params0, mm)
    val result0 = autotune.execution.execute(mm0, main(1024, 1024, 1024), Timeouts(5000, 5000, 5000), 10, 100)
    println("result0: " + result0.runtime)

    val params1 = Map(
      TuningParameter("n") -> (1024: Nat),
      TuningParameter("m") -> (1024: Nat),
      TuningParameter("o") -> (1024: Nat),
      TuningParameter("ls0") -> (4: Nat),
      TuningParameter("ls1") -> (32: Nat),
      TuningParameter("gs0") -> (128: Nat),
      TuningParameter("gs1") -> (64: Nat),
      TuningParameter("v3") -> (4: Nat),
      TuningParameter("v4") -> (1: Nat),
      TuningParameter("v5") -> (32: Nat),
      TuningParameter("v6") -> (128: Nat),
      TuningParameter("v7") -> (8: Nat),
      TuningParameter("v8") -> (128: Nat)
    )

    val mm1 = rise.core.substitute.natsInExpr(params1, mm)
    val result1 = autotune.execution.execute(mm1, main(1024, 1024, 1024), Timeouts(5000, 5000, 5000), 10, 100)
    println("result1: " + result1.runtime)

    val params2 = Map(
      TuningParameter("n") -> (1024: Nat),
      TuningParameter("m") -> (1024: Nat),
      TuningParameter("o") -> (1024: Nat),
      TuningParameter("ls0") -> (8: Nat),
      TuningParameter("ls1") -> (128: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1024: Nat),
      TuningParameter("v3") -> (4: Nat),
      TuningParameter("v4") -> (1: Nat),
      TuningParameter("v5") -> (32: Nat),
      TuningParameter("v6") -> (256: Nat),
      TuningParameter("v7") -> (64: Nat),
      TuningParameter("v8") -> (32: Nat)
    )

    val mm2 = rise.core.substitute.natsInExpr(params2, mm)
    val result2 = autotune.execution.execute(mm2, main(1024, 1024, 1024), Timeouts(5000, 5000, 5000), 10, 100)
    println("result2: " + result2.runtime)
  }

  test("mm tuning 128"){
    val mm = tuningParam("ls0", (ls0: Nat) => tuningParam("ls1", (ls1: Nat) =>
      tuningParam("gs0", (gs0: Nat) => tuningParam("gs1", (gs1: Nat) =>
        wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning)))))

    val tuner = Tuner(main(64, 128, 128), 100, "rs_cot_128", "autotuning/mm_128", Timeouts(5000, 5000, 1000), 10, 100, Some("/home/jo/development/rise-lang/shine/autotuning/config/rs_cot_128.json"), true)

    val tuningResult = autotune.search(tuner)(mm)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: "+ bestSample)
    println("runtime: " + bestSample.get.runtime)
  }


  test("mm tuning 1024"){
    val mm: Expr =
      tuningParam("ls0", (ls0: Nat) => tuningParam("ls1", (ls1: Nat) =>
        tuningParam("gs0", (gs0: Nat) => tuningParam("gs1", (gs1: Nat) =>
          wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning)
        ))))

    val tuner = Tuner(main(1024, 1024, 1024), 100, "rs_cot_1024", "autotuning/mm_1024", Timeouts(1000, 1000, 1000), 10, 100, Some("/home/jo/development/rise-lang/shine/autotuning/config/rs_cot_1024.json"), true)

    val tuningResult = autotune.search(tuner)(mm)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }


}
