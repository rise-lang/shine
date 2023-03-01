package apps.autotuning

import apps.mm.mmNVIDIAWithParams
import arithexpr.arithmetic.{RangeAdd, RangeMul}
import rise.autotune
import rise.autotune._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}

import scala.language.postfixOps

import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune._
import rise.core.DSL.HighLevelConstructs.{padCst2D, slide2D}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives.{add, asScalar, asVectorAligned, join, mapSeq, split, vectorFromScalar}
import rise.core.types.DataType.{ArrayType, f32}
import rise.core.types.{AddressSpace, Nat, TuningParameter}
import rise.openCL.DSL.mapGlobal
import rise.openCL.primitives.oclReduceSeq
import shine.OpenCL.{GlobalSize, LocalSize}

class constraintsEvaluation extends test_util.Tests {

  import rise.openCL.DSL._

  val inputSize = 1 << 25

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

  val scalVec =
    tuningParam("s0", RangeMul(1, inputSize, 2), (s0: Nat) =>
      tuningParam("s1", RangeMul(1, inputSize, 2), (s1: Nat) =>
        tuningParam("vec", RangeMul(1, 1024, 2), (vec: Nat) =>
          depFun((n: Nat) => fun(n `.` f32)(input => fun(f32)(alpha =>
            input |>
              split(s0) |>
              mapWorkGroup(
                asVectorAligned(vec) >>
                  split(s1) >>
                  mapLocal(mapSeq(fun(x => vectorFromScalar(alpha) * x))) >>
                  join >> asScalar
              ) |>
              join
          )))
        )))

  val scalOcl: Expr =
    tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
      tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
        tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
          tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(scalVec)
          ))))


  // scalastyle:off
  object mm_host_code
  {
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
  }


  // hostcode
  object scal_host_code
  {
    val init: Int => String = N => {
      s"""
         |const int N = ${N};
         |
         |srand(time(NULL));
         |
         |Buffer input = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
         |Buffer output = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
         |
         |float* m = hostBufferSync(ctx, input, N * sizeof(float), HOST_WRITE);
         |for (int i = 0; i < N; i++) {
         |  // m[i] = (float)(rand()) * 10.0f;
         |  m[i] = 1.0f;
         |}
         |
         |int alpha = 10;
         |
         |// synchronize before entering timed section
         |deviceBufferSync(ctx, input, N * sizeof(float), DEVICE_READ);
         |waitFinished(ctx);
         |""".stripMargin
    }

    val compute =
      s"""
         |fun_run(ctx, &fun, output, N, input, alpha);
         |waitFinished(ctx);
         |""".stripMargin

    val finish =
      s"""
         |// TODO: could check output here
         |// use given gold expression?
         |
         |//float* outputScal = hostBufferSync(ctx, output, N * sizeof(float), HOST_READ);
         |//for(int i = 0; i < N; i++){
         | // printf("%f \\n", outputScal[i]);
         |//}
         |
         |destroyBuffer(ctx, input);
         |destroyBuffer(ctx, output);
         |""".stripMargin
  }


  test("constraints test mm") {
    val inputSize: Int = 1024

//    val configs = Seq(
//      s"autotuning/config/constraints/mm/mm_base.json",
//    )
//
//    runExperiment(
//      name = s"mm_${inputSize}",
//      configFiles = configs,
//      iterations = 30,
//      output = s"experiment/results/paper/constraints/mm",
//      e = mm,
//      hostCode = HostCode(mm_host_code.init(inputSize, inputSize, inputSize), mm_host_code.compute, mm_host_code.finish),
//      inputSizes = Seq(inputSize, inputSize, inputSize),
//      plotOnly = false,
//      expert = None,
//      default = None,
//        feasibility = false
//    )

    val configs2 = Seq(
      s"autotuning/config/constraints/mm/mm_faes.json",
      s"autotuning/config/constraints/mm/mm_faes0.json",
    )

    runExperiment(
      name = s"mm_${inputSize}",
      configFiles = configs2,
      iterations = 30,
      output = s"experiment/results/paper/constraints/mm",
      e = mm,
      hostCode = HostCode(mm_host_code.init(inputSize, inputSize, inputSize), mm_host_code.compute, mm_host_code.finish),
      inputSizes = Seq(inputSize, inputSize, inputSize),
      plotOnly = false,
      expert = None,
      default = None,
    )

  }



  ignore("constraints test scal") {
    val inputSize: Int = 1 << 25
    val inputSize2: Int = 1 << 25

    val configs = Seq(
      s"autotuning/config/constraints/scal/scal_faes.json",
      s"autotuning/config/constraints/scal/scal_faes0.json"
    )

    runExperiment(
      name = s"scal_${inputSize}",
      configFiles = configs,
      iterations = 30,
      output = s"experiment/results/paper/constraints/scal",
      e = scalOcl,
      hostCode = HostCode(scal_host_code.init(inputSize2), scal_host_code.compute, scal_host_code.finish),
      inputSizes = Seq(inputSize2),
      expert = None,
      default = None,
      disableChecking = true
    )

    val configs2 = Seq(
      s"autotuning/config/constraints/scal/scal_base.json",
    )
//

    runExperiment(
      name = s"scal_${inputSize}",
      configFiles = configs2,
      iterations = 30,
      output = s"experiment/results/paper/constraints/scal",
      e = scalOcl,
      hostCode = HostCode(scal_host_code.init(inputSize2), scal_host_code.compute, scal_host_code.finish),
      inputSizes = Seq(inputSize2),
      expert = None,
      default = None,
      disableChecking = true,
      feasibility = false
    )
  }


  // todo
  // add mode/option, where we just return a high value for failing expressions -> not tuple


}
