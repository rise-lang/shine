package apps.autotuning.fair_embedding_evaluation

import apps.autotuning._
import apps.mm.mmNVIDIAWithParams
import arithexpr.arithmetic.{RangeAdd, RangeMul}
import rise.autotune
import rise.autotune._
import rise.core.DSL._
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}

import scala.language.postfixOps

class mmEmbedding extends test_util.Tests {

  val mmTuning: ToBeTyped[Expr] =
    tuningParam("v3", RangeAdd(1, 1024, 1), (v3: Nat) =>
      tuningParam("v4", RangeAdd(1, 1024, 1), (v4: Nat) =>
        tuningParam("v5", RangeAdd(1, 1024, 1), (v5: Nat) =>
          tuningParam("v6", RangeAdd(1, 1024, 1), (v6: Nat) =>
            tuningParam("v7", RangeAdd(1, 1024, 1), (v7: Nat) =>
              tuningParam("v8", RangeAdd(1, 1024, 1), (v8: Nat) =>
                mmNVIDIAWithParams(v3, v4, v5, v6, v7, v8)
              ))))))

  val mm: Expr =
    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
      tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
        tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
          tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning)
          ))))

  // scalastyle:off
  val init: (Int, Int, Int) => String = (N, M, O) => {
    s"""
       |const int N = ${N};
       |const int M = ${M};
       |const int O = ${O};
       |
       |srand(time(NULL));
       |
       |Buffer inputA = createBuffer(ctx, N * M * sizeof(float), HOST_WRITE | DEVICE_READ);
       |Buffer inputB = createBuffer(ctx, M * O * sizeof(float), HOST_WRITE | DEVICE_READ);
       |Buffer outputC = createBuffer(ctx, N * O *  sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |float* inA = hostBufferSync(ctx, inputA, N * M * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N * M ; i++) {
       |  //inA[i] = (float)(rand());
       |  inA[i] = (float)(i+1);
       |}
       |
       |float* inB = hostBufferSync(ctx, inputB, M * O * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < M * O; i++) {
       |  //inB[i] = (float)(rand());
       |  inB[i] = (float)(i+1);
       |}
       |
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, outputC, M, N, O, inputA, inputB);
       |""".stripMargin

  val finish =
    s"""
       |// TODO: could check output here
       |
       |destroyBuffer(ctx, inputA);
       |destroyBuffer(ctx, inputB);
       |destroyBuffer(ctx, outputC);
       |""".stripMargin
  // scalastyle:on

  test("execute expert configuration") {
    // execute config with "expert parameter configuration"
    val mm: Expr =
      tuningParam("ls0", (ls0: Nat) => tuningParam("ls1", (ls1: Nat) =>
        tuningParam("gs0", (gs0: Nat) => tuningParam("gs1", (gs1: Nat) =>
          wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning)))))

    // expert config for 128x64 * 128x128
    val params0: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (32: Nat),
      TuningParameter("ls1") -> (8: Nat),
      TuningParameter("gs0") -> (256: Nat),
      TuningParameter("gs1") -> (128: Nat),
      TuningParameter("v3") -> (4: Nat),
      TuningParameter("v4") -> (8: Nat),
      TuningParameter("v5") -> (64: Nat), // tile-width A
      TuningParameter("v6") -> (128: Nat), // divides v8 x v5
      TuningParameter("v7") -> (128: Nat), // tile-width B
      TuningParameter("v8") -> (16: Nat) // tile-height A,B
    )

    val mm0 = rise.core.substitute.natsInExpr(params0, mm)
    val result0 = autotune.execution.execute(
      expression = mm0,
      hostCode = HostCode(init(1024, 1024, 1024), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 100,
      speedupFactor = 100,
      execution = Median
    )
    println("result0: " + result0.runtime)
    //    assert(result0.runtime.isRight)

    // expert config for 128x64 * 128x128
    val params1: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (16: Nat),
      TuningParameter("ls1") -> (16: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1024: Nat),
      TuningParameter("v3") -> (1: Nat),
      TuningParameter("v4") -> (1: Nat),
      TuningParameter("v5") -> (32: Nat), // tile-width A
      TuningParameter("v6") -> (32: Nat), // divides v8 x v5
      TuningParameter("v7") -> (32: Nat), // tile-width B
      TuningParameter("v8") -> (32: Nat) // tile-height A,B
    )

    val mm1 = rise.core.substitute.natsInExpr(params1, mm)
    val result1 = autotune.execution.execute(
      expression = mm1,
      hostCode = HostCode(init(1024, 1024, 1024), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 100,
      speedupFactor = 100,
      execution = Median
    )
    println("result1: " + result1.runtime)
    assert(result1.runtime.isRight)

  }

  test("tune mm 1024") {
    val inputSize: Int = 1024

    // expert configuration
    val expertConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (32: Nat),
      TuningParameter("ls1") -> (8: Nat),
      TuningParameter("gs0") -> (256: Nat),
      TuningParameter("gs1") -> (128: Nat),
      TuningParameter("v3") -> (4: Nat),
      TuningParameter("v4") -> (8: Nat),
      TuningParameter("v5") -> (64: Nat), // tile-width A
      TuningParameter("v6") -> (128: Nat), // divides v8 x v5
      TuningParameter("v7") -> (128: Nat), // tile-width B
      TuningParameter("v8") -> (16: Nat) // tile-height A,B
    )

    val defaultConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (1: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1024: Nat),
      TuningParameter("v3") -> (4: Nat),
      TuningParameter("v4") -> (1: Nat),
      TuningParameter("v5") -> (4: Nat), // tile-width A
      TuningParameter("v6") -> (4: Nat), // divides v8 x v5
      TuningParameter("v7") -> (4: Nat), // tile-width B
      TuningParameter("v8") -> (1: Nat) // tile-height A,B
    )

    val configs = Seq(
      s"autotuning/fair_embedding_evaluation/mm/opentuner.json",
      s"autotuning/fair_embedding_evaluation/mm/opentuner_biased.json",
      s"autotuning/fair_embedding_evaluation/mm/embedding_random_sampling.json",
      s"autotuning/fair_embedding_evaluation/mm/embedding_random_sampling_biased.json"
    )

    runExperiment(
      name = s"mm",
      configFiles = configs,
      iterations = 30,
      output = s"experiment/results/fair_embedding_evaluation",
      e = mm,
      hostCode = HostCode(init(inputSize, inputSize, inputSize), compute, finish),
      inputSizes = Seq(inputSize, inputSize, inputSize),
      plotOnly = false,
      expert = Some(expertConfiguration),
      default = Some(defaultConfiguration)
    )
  }
}
