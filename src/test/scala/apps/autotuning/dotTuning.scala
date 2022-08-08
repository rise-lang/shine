package apps.autotuning

import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types.DataType.{ArrayType, f32}
import rise.core.types.{AddressSpace, Nat}
import rise.openCL.primitives.{oclIterate, oclReduceSeq}
import shine.OpenCL.{GlobalSize, LocalSize}

class dotTuning extends test_util.Tests {

  import rise.openCL.DSL._


  private def xsT(N: Nat) = ArrayType(N, f32)

  private def ysT(N: Nat) = ArrayType(N, f32)

  private val mulT = fun(x => fst(x) * snd(x))
  private val add = fun(a => fun(x => a + x))


  val dot = depFun((n: Nat) => fun(xsT(n))(in =>
    in |>
      split(128) |>
      mapWorkGroup(
        split(2) >>
          toLocalFun(
            mapLocal(oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f)))
          ) >>
          toLocalFun(
            oclIterate(AddressSpace.Local)(6)(depFun((_: Nat) =>
              split(2) >> mapLocal(oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f)))
            ))
          ) >> mapLocal(fun(x => x))
      ) |> join
  ))


  val dotOcl: Expr =
    tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
      tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
        tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
          tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(dot)
          ))))



  // todo adjsut hostcode

  // hostcode
  val init: Int => String = N => {
    s"""
       |const int N = ${N};
       |
       |srand(time(NULL));
       |
       |Buffer inputA = createBuffer(ctx, N * N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer inputB = createBuffer(ctx, N * N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer output = createBuffer(ctx, N * N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
       |
       |float* inputa = hostBufferSync(ctx, inputA, N * N * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N * N; i++) {
       |  inputa[i] = (float)(rand())/(float)(RAND_MAX) * 10.0f;
       |}
       |float* inputb = hostBufferSync(ctx, inputB, N * N * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N * N; i++) {
       |  inputb[i] = (float)(rand())/(float)(RAND_MAX) * 10.0f;
       |}
       |
       |// synchronize before entering timed section
       |//deviceBufferSync(ctx, input, N * N * sizeof(float), DEVICE_READ);
       |//waitFinished(ctx);
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, output, N, inputA);
       |waitFinished(ctx);
       |""".stripMargin

  val finish =
    s"""
       |// TODO: could check output here
       |// use given gold expression?
       |
       |destroyBuffer(ctx, input);
       |destroyBuffer(ctx, output);
       |""".stripMargin


  test("execute dot") {


    // todo check dependend fun and normal fun (injecting parametrs)
    // todo check kernel args

    val inputSize: Int = 1024
    val e: Expr = dot

    println("Expression: \n" + e)

    val eOcl = wrapOclRun(LocalSize(2), GlobalSize(1024))(e)
    //        val eOcl = e

    //    println("Expression: \n" + eOcl)

    val result = rise.autotune.execution.execute(
      expression = eOcl,
      hostCode = HostCode(init(inputSize), compute, finish),
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 100,
      speedupFactor = 100,
      execution = Median
    )

    println("result: \n" + result)

  }


  test("dot tuning experiment") {
    val inputSize: Int = 1024

    val tuner = Tuner(
      hostCode = HostCode(init(inputSize), compute, finish),
      inputSizes = Seq(inputSize),
      samples = 20, // defined by config file, value is ignored
      name = "scal",
      output = "autotuning",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = None,
      hmConstraints = true,
      runtimeStatistic = Minimum,
      saveToFile = true
    )

    autotune.search(tuner)(dotOcl)

  }


  test("tune dot 1024") {
    val inputSize: Int = 1024
    val inputSize2: Int = 1024

    val configs = Seq(
      s"autotuning/config/scal/${inputSize.toString}/rs_cot_${inputSize.toString}.json",
      //      s"autotuning/config/scal/${inputSize.toString}/rs_emb_${inputSize.toString}.json",
      //      s"autotuning/config/scal/${inputSize.toString}/bogp_cot_${inputSize.toString}.json",
      //      s"autotuning/config/scal/${inputSize.toString}/atf_emb_${inputSize.toString}.json"
    )


    runExperiment(
      name = s"scal_${inputSize}",
      configFiles = configs,
      iterations = 2,
      output = s"autotuning/scal_${inputSize}",
      e = dotOcl,
      hostCode = HostCode(init(inputSize2), compute, finish),
      inputSizes = Seq(inputSize2),
      disableChecking = true
    )
  }

}
