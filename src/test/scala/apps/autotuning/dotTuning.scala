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

import rise.core.DSL.HighLevelConstructs.reorderWithStride

class dotTuning extends test_util.Tests {

  import rise.openCL.DSL._

  private def xsT(N: Nat) = ArrayType(N, f32)

  private def ysT(N: Nat) = ArrayType(N, f32)

  private val mulT = fun(x => fst(x) * snd(x))
  private val add = fun(a => fun(x => a + x))

  val dot1 = depFun((n: Nat) => fun(xsT(n))(in =>
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

  val dot2 = depFun((n: Nat) => fun(xsT(n))(xs => fun(ysT(n))(ys =>
    zip(xs)(ys) |>
      split(2048 * 128) |>
      mapWorkGroup(
        reorderWithStride(128) >>
          split(2048) >>
          mapLocal(
            oclReduceSeq(AddressSpace.Private)(
              fun(a => fun(x => a + mulT(x)))
            )(lf32(0.0f))
          )
      ) |> join
  )))


  val dotOcl: Expr =
    tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
      tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
        tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
          tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(dot)
          ))))


  val dot = dot1


  // todo adjsut hostcode

  // hostcode
  val init: Int => String = N => {
    s"""
       |const int N = ${N};
       |
       |srand(time(NULL));
       |
       |Buffer inputA = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |// Buffer inputB = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer output = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
       |
       |float* inputa = hostBufferSync(ctx, inputA, N * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N; i++) {
       |  //inputa[i] = (float)(rand())/(float)(RAND_MAX) * 10.0f;
       |  inputa[i] = 1;
       |}
       |//float* inputb = hostBufferSync(ctx, inputB, N * sizeof(float), HOST_WRITE);
       |//for (int i = 0; i < N * N; i++) {
       | // inputb[i] = (float)(rand())/(float)(RAND_MAX) * 10.0f;
       |//}
       |
       |float* outputDot = hostBufferSync(ctx, output, N * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N; i++) {
       |  //inputa[i] = (float)(rand())/(float)(RAND_MAX) * 10.0f;
       |  outputDot[i] = 0;
       |}
       |
       |// synchronize before entering timed section
       |//deviceBufferSync(ctx, input, N * sizeof(float), DEVICE_READ);
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
       |//deviceBufferSync(ctx, output, N * sizeof(float), DEVICE_READ);
       |
       |//for(int i = 0; i < N; i++){
       | // printf("%f \\n", inputa[i]);
       |//}
       |
       |//float* outputDot = hostBufferSync(ctx, output, N * sizeof(float), HOST_READ);
       |for(int i = 0; i < N; i++){
       | printf("%f \\n", outputDot[i]);
       |}
       |destroyBuffer(ctx, inputA);
       |destroyBuffer(ctx, output);
       |""".stripMargin


  // todo inject local and globalsize
  val manualHostCode: (Int, Int) => String = (LS0, GS0) =>
    s"""
       |#include "ocl/ocl.h"
       |struct fun_t {
       |  Kernel k0;
       |};
       |
       |typedef struct fun_t fun_t;
       |
       |void fun_init(Context ctx, fun_t* self){
       |  (*self).k0 = loadKernel(ctx, k0);
       |}
       |
       |void fun_destroy(Context ctx, fun_t* self){
       |  destroyKernel(ctx, (*self).k0);
       |}
       |
       |void fun_run(Context ctx, fun_t* self, Buffer moutput, int n4, Buffer me5){
       |  {
       |    DeviceBuffer b0 = deviceBufferSync(ctx, moutput, (n4 / ${LS0}) * sizeof(float), DEVICE_WRITE);
       |    DeviceBuffer b3 = deviceBufferSync(ctx, me5, n4 * sizeof(float), DEVICE_READ);
       |    const size_t global_size[3] = (const size_t[3]){${GS0}, 1, 1};
       |    const size_t local_size[3] = (const size_t[3]){${LS0}, 1, 1};
       |    const KernelArg args[5] = (const KernelArg[5]){KARG(b0), KARG(n4), KARG(b3), LARG(64 * sizeof(float)), LARG(1 * sizeof(float))};
       |    launchKernel(ctx, (*self).k0, global_size, local_size, 5, args);
       |  }
       |
       |}
       |
       |void fun_init_run(Context ctx, Buffer moutput, int n4, Buffer me5){
       |  fun_t fun;
       |  fun_init(ctx, &fun);
       |  fun_run(ctx, &fun, moutput, n4, me5);
       |  fun_destroy(ctx, &fun);
       |}
       |
       |""".stripMargin

  val magister =
    s"""
       |__kernel
       |void k0(global float* restrict output, int n4, const global float* restrict e5, local float* restrict x297, local float* restrict x276){
       |  /* Start of moved local vars */
       |  local float tmp2_323[64];
       |  local float tmp1_322[64];
       |  /* End of moved local vars */
       |  /* mapWorkGroup */
       |  for (int wg_id_319 = get_group_id(0); wg_id_319 < (n4 / 128); wg_id_319 = wg_id_319 + get_num_groups(0)) {
       |    /* mapLocal */
       |    for (int l_id_320 = get_local_id(0); l_id_320 < 64; l_id_320 = l_id_320 + get_local_size(0)) {
       |      /* oclReduceSeq */
       |      {
       |        float x304;
       |        x304 = 0.0f;
       |        for (int i_321 = 0; i_321 < 2; i_321 = 1 + i_321) {
       |          x304 = x304 + e5[(i_321 + (2 * l_id_320)) + (128 * wg_id_319)];
       |        }
       |
       |        x297[l_id_320] = x304;
       |      }
       |
       |    }
       |
       |    barrier(CLK_LOCAL_MEM_FENCE);
       |    {
       |      /* tmp1_322 moved */
       |      /* tmp2_323 moved */
       |      local float* in_ptr_324 = &x297[0];
       |      local float* out_ptr_325 = tmp1_322;
       |      unsigned char flag_326 = 1;
       |      for (int i_327 = 0; i_327 < 6; i_327 = 1 + i_327) {
       |        /* mapLocal */
       |        for (int l_id_328 = get_local_id(0); l_id_328 < (1 << (5 + (-1 * i_327))); l_id_328 = l_id_328 + get_local_size(0)) {
       |          /* oclReduceSeq */
       |          {
       |            float x285;
       |            x285 = 0.0f;
       |            for (int i_329 = 0; i_329 < 2; i_329 = 1 + i_329) {
       |              x285 = x285 + in_ptr_324[i_329 + (2 * l_id_328)];
       |            }
       |
       |            out_ptr_325[l_id_328] = x285;
       |          }
       |
       |        }
       |
       |        if (i_327 < 4) {
       |          {
       |            in_ptr_324 = flag_326 ? tmp1_322 : tmp2_323;
       |            out_ptr_325 = flag_326 ? tmp2_323 : tmp1_322;
       |            flag_326 = flag_326 ^ 1;
       |          }
       |
       |        }
       |         else {
       |          {
       |            in_ptr_324 = flag_326 ? tmp1_322 : tmp2_323;
       |            out_ptr_325 = &x276[0];
       |          }
       |
       |        }
       |
       |        barrier(CLK_LOCAL_MEM_FENCE);
       |      }
       |
       |    }
       |
       |    /* mapLocal */
       |    for (int l_id_330 = get_local_id(0); l_id_330 < 1; l_id_330 = l_id_330 + get_local_size(0)) {
       |      output[l_id_330 + wg_id_319] = x276[l_id_330];
       |    }
       |
       |    barrier(CLK_LOCAL_MEM_FENCE);
       |  }
       |
       |}
       |
       |""".stripMargin


  test("execute dot") {

    // todo
    // does not compute the right thing


    // magister
    val magister = util.gen.opencl.kernel.apply("k0").asStringFromExpr(dot)
    val module =
      s"""const char k0_source[] =""" + "\n" +
        magister.split("\n").map(elem => s""" "${elem} " """).reduce((a, b) => a + "\n" + b) + "\n" + ";" + "\n" +
        s"""#define loadKernel(ctx, id)\\
      loadKernelFromSource(ctx, #id, id##_source, sizeof(id##_source) - 1)""" + "\n" +
        manualHostCode(128, 1024) + "\n"

    val program =
      s"""
         |${module}
         |
         |int main(int argc, char** argv) {
         |  Context ctx = createDefaultContext();
         |  fun_t fun;
         |  fun_init(ctx, &fun);
         |
         |  ${init(1024)}
         |
         |  int iterations = atoi(argv[1]);
         |  for (int sample = 0; sample < iterations; sample++) {
         |    ${compute}
         |  }
         |  ${finish}
         |  fun_destroy(ctx, &fun);
         |  destroyContext(ctx);
         |  return EXIT_SUCCESS;
         |}
         |""".stripMargin


    println("program: \n" + program)


    // write code to file

    util.writeToPath("autotuning/generated/dot.c", program)

    // create kernel from dot expression


    val kernel = util.gen.opencl.kernel.asStringFromExpr(dot)
    println("kernel: \n" + kernel)


    //      gen.opencl. .fromExpr(expression))

    // todo check dependend fun and normal fun (injecting parametrs)
    // todo check kernel args

    val inputSize: Int = 8192
    val e: Expr = dot

    println("Expression: \n" + e)

    val eOcl = wrapOclRun(LocalSize(128), GlobalSize(1024))(e)
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
