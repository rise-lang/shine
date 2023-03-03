package apps.autotuning

import apps.nbody._
import arithexpr.arithmetic.{RangeAdd, RangeMul, RangeUnknown}
import rise.autotune
import rise.autotune.{HostCode, Median, Minimum, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen

class nBodyTuning extends test_util.Tests {

  val nbodyNoTuning = nbodyNVIDIAWithParams(256, 1)

  val nbodyTuning: ToBeTyped[Expr] =
  //    tuningParam("tileX", RangeMul(1, 1024, 2), (tileX: Nat) =>
    tuningParam("tileX", RangeUnknown, (tileX: Nat) =>
      //      tuningParam("tileY", RangeMul(1, 1024, 2), (tileY: Nat) =>
      tuningParam("tileY", RangeUnknown, (tileY: Nat) =>
        //        tuningParam("vec", RangeMul(1, 1024, 2), (vec: Nat) =>
        nbodyNVIDIAWithParams(tileX, tileY)
      ))

  // scalastyle:off
  val init: (Int) => String = (N) => {
    s"""
       |  const int N = ${N};
       |  srand(time(NULL));
       |  Buffer pos = createBuffer(ctx, 4 * N * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer vel = createBuffer(ctx, 4 * N * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer output = createBuffer(ctx, 4 * N * 2 * sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |  float* in_pos = hostBufferSync(ctx, pos, 4 * N * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < 4 * N; i++) {
       |    in_pos[i] = (float)(rand() % 100);
       |  }
       |
       |  float* in_vel = hostBufferSync(ctx, vel, 4 * N * sizeof(float), HOST_WRITE);
       |    for (int i = 0; i < 4 * N; i++) {
       |      in_vel[i] = (float)(rand() % 100);
       |  }
       |
       |  float deltaT = 0.005;
       |  float espSqr = 500.0;
       |
       |  deviceBufferSync(ctx, features, 4 * N * sizeof(float), DEVICE_READ);
       |  deviceBufferSync(ctx, clusters, 4 * N * sizeof(float), DEVICE_READ);
       |""".stripMargin
  }
  val compute =
    s"""
       |    fun_init_run(ctx, output, N, pos, vel, deltaT, espSqr);
       |""".stripMargin

  val finish =
    s"""
       |  // could add error checking
       |  destroyBuffer(ctx, features);
       |  destroyBuffer(ctx, clusters);
       |  destroyBuffer(ctx, output);
       |""".stripMargin
  // scalastyle:on


  test("generate kernel nBody") {

    val code = gen.opencl.kernel(Some(nbodyNVIDIAKnownSizes), "KERNEL").asStringFromExpr(nbodyNVIDIA)

    // combine this with hostCode?
    // hostCode takes paraemters?

    println("code: \n" + code)

    //    code.foreach(elem => {
    //      println("kernel: \n" + )
    //    })

    System.exit(0)


  }


  // bypass host code generation by assembling stuff by hand
  test("bypass host code generation"){

    // add code here

    // how looks generated module?
    val generatedModule = gen.opencl.hosted("fun")
    val code = gen.opencl.kernel(Some(nbodyNVIDIAKnownSizes), "KERNEL").asStringFromExpr(nbodyNVIDIA)
    //    val code = gen.opencl.kernel(Some(nbodyNVIDIAKnownSizes), "KERNEL").asStringFromExpr(nbodyNVIDIA)

    println("code: \n" + code)

    val preparation: String =
      s"""
         |struct Record_float4_float4 {
         |  float4 _fst;
         |  float4 _snd;
         |};
         |
         |float4 calcAcc(float4 p1, float4 p2, float deltaT, float espSqr, float4 acc){
         |  float4 r;
         |  r.xyz = p2.xyz - p1.xyz;
         |  float distSqr = r.x*r.x + r.y*r.y + r.z*r.z;
         |  float invDist = 1.0f / sqrt(distSqr + espSqr);
         |  float invDistCube = invDist * invDist * invDist;
         |  float s = invDistCube * p2.w;
         |  float4 res;
         |  res.xyz = acc.xyz + s * r.xyz;
         |  return res;
         |}
         |
         |struct Record_float4_float4 update(float4 pos, float4 vel, float deltaT, float4 acceleration){
         |  float4 newPos;
         |  newPos.xyz = pos.xyz + vel.xyz * deltaT + 0.5f * acceleration.xyz * deltaT * deltaT;
         |  newPos.w = pos.w;
         |  float4 newVel;
         |  newVel.xyz = vel.xyz + acceleration.xyz * deltaT;
         |  newVel.w = vel.w;
         |  return (struct Record_float4_float4){ newPos, newVel };
         |}
         |""".stripMargin

    val kernel: String =
      s"""
         |__kernel __attribute__ ((reqd_work_group_size(256, 1, 1)))
         |void KERNEL(global struct Record_float4_float4* restrict output, int n37, const global float4* restrict e38, const global float4* restrict e39, float e40, float e41, local float4* restrict x738){
         |  /* Start of moved local vars */
         |  /* End of moved local vars */
         |  /* mapWorkGroup */
         |  /* iteration count is exactly 1, no loop emitted */
         |  int wg_id_763 = get_group_id(1);
         |  /* mapWorkGroup */
         |  /* iteration count is exactly 1, no loop emitted */
         |  int wg_id_764 = get_group_id(0);
         |  /* oclReduceSeq */
         |  {
         |    float4 x654[256];
         |    /* mapLocal */
         |    /* unrolling loop of 1 */
         |    /* mapLocal */
         |    /* unrolling loop of 1 */
         |    x654[(256 * get_local_id(1)) + get_local_id(0)] = (float4)(0.0f);
         |    for (int i_765 = 0; i_765 < (n37 / 256); i_765 = 1 + i_765) {
         |      /* mapLocal */
         |      /* iteration count is exactly 1, no loop emitted */
         |      int l_id_766 = get_local_id(1);
         |      /* mapLocal */
         |      /* iteration count is exactly 1, no loop emitted */
         |      int l_id_767 = get_local_id(0);
         |      x738[l_id_767 + (256 * l_id_766)] = e38[(l_id_767 + (256 * i_765)) + (256 * l_id_766)];
         |      barrier(CLK_LOCAL_MEM_FENCE);
         |      /* mapLocal */
         |      /* unrolling loop of 1 */
         |      {
         |        float4 x718[1];
         |        /* mapLocal */
         |        /* unrolling loop of 1 */
         |        x718[0] = e38[((256 * wg_id_764) + (n37 * wg_id_763)) + get_local_id(0)];
         |        /* mapLocal */
         |        /* unrolling loop of 1 */
         |        /* oclReduceSeq */
         |        {
         |          float4 x686;
         |          x686 = x654[(256 * get_local_id(1)) + get_local_id(0)];
         |          for (int i_768 = 0; i_768 < 256; i_768 = 1 + i_768) {
         |            x686 = calcAcc(x718[0], x738[i_768 + (256 * get_local_id(1))], e41, e40, x686);
         |          }
         |
         |          x654[(256 * get_local_id(1)) + get_local_id(0)] = x686;
         |        }
         |
         |      }
         |
         |      barrier(CLK_LOCAL_MEM_FENCE);
         |    }
         |
         |    /* mapLocal */
         |    /* unrolling loop of 1 */
         |    {
         |      float4 x640[1];
         |      /* mapLocal */
         |      /* unrolling loop of 1 */
         |      x640[0] = e38[((256 * wg_id_764) + (n37 * wg_id_763)) + get_local_id(0)];
         |      /* mapLocal */
         |      /* unrolling loop of 1 */
         |      {
         |        struct Record_float4_float4 x618;
         |        x618 = update(x640[0], e39[((256 * wg_id_764) + (n37 * wg_id_763)) + get_local_id(0)], e41, x654[(256 * get_local_id(1)) + get_local_id(0)]);
         |        output[(((256 * wg_id_764) + (256 * get_local_id(1))) + (n37 * wg_id_763)) + get_local_id(0)]._fst = x618._fst;
         |        output[(((256 * wg_id_764) + (256 * get_local_id(1))) + (n37 * wg_id_763)) + get_local_id(0)]._snd = x618._snd;
         |      }
         |
         |    }
         |
         |  }
         |
         |}
         |
         |""".stripMargin

    val wrapping: String =
      s"""
         |
         |#define loadKernel(ctx, id) \\
         loadKernelFromSource(ctx, #id, id##_source, sizeof(id##_source) - 1)
         |
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
         |""".stripMargin

    // todo check and adjust
    // todo check inputs and buffers
    val wrapping2: String =
      s"""
         |void fun_run(Context ctx, fun_t* self, Buffer moutput, int n204, int n205, int n206, Buffer me207, Buffer me208){
         |  {
         |    DeviceBuffer b0 = deviceBufferSync(ctx, moutput, n204 * (n205 * sizeof(float)), DEVICE_WRITE);
         |    DeviceBuffer b4 = deviceBufferSync(ctx, me207, n206 * (n204 * sizeof(float)), DEVICE_READ);
         |    DeviceBuffer b5 = deviceBufferSync(ctx, me208, n206 * (n205 * sizeof(float)), DEVICE_READ);
         |    const size_t global_size[3] = (const size_t[3]){256, 128, 1};
         |    const size_t local_size[3] = (const size_t[3]){32, 8, 1};
         |    const KernelArg args[7] = (const KernelArg[7]){KARG(b0), KARG(n205), KARG(n204), KARG(n206), KARG(b4), KARG(b5), LARG((1 <= (n206 / 16) ? 1 : (n206 / 16)) * ((16 * (64 * sizeof(float))) + (16 * (128 * sizeof(float)))))};
         |    launchKernel(ctx, (*self).k0, global_size, local_size, 7, args);
         |  }
         |
         |}
         |
         |void fun_init_run(Context ctx, Buffer moutput, int n204, int n205, int n206, Buffer me207, Buffer me208){
         |  fun_t fun;
         |  fun_init(ctx, &fun);
         |  fun_run(ctx, &fun, moutput, n204, n205, n206, me207, me208);
         |  fun_destroy(ctx, &fun);
         |}
         |""".stripMargin


    // todo check and adjust
    val main: String =
      s"""
         |int main(int argc, char** argv) {
         |  Context ctx = createDefaultContext();
         |  fun_t fun;
         |  fun_init(ctx, &fun);
         |
         |
         |const int N = 1024;
         |const int M = 1024;
         |const int O = 1024;
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
         |
         |
         |  int iterations = atoi(argv[1]);
         |  for (int sample = 0; sample < iterations; sample++) {
         |
         |fun_run(ctx, &fun, outputC, M, N, O, inputA, inputB);
         |
         |  }
         |
         |// TODO: could check output here
         |
         |destroyBuffer(ctx, inputA);
         |destroyBuffer(ctx, inputB);
         |destroyBuffer(ctx, outputC);
         |
         |  fun_destroy(ctx, &fun);
         |  destroyContext(ctx);
         |  return EXIT_SUCCESS;
         |}
         |""".stripMargin


    // todo

    // assemble, compile and execute

//    println("generatedModule: \n" + generatedModule)

//    val code = gen.opencl.kernel(Some(nbodyNVIDIAKnownSizes), "KERNEL").asStringFromExpr(nbodyNVIDIA)
//
//    val program = {
//      s"""
//         |${shine.OpenCL.Module.translateToString(generatedModule)}
//         |
//         |int main(int argc, char** argv) {
//         |  Context ctx = createDefaultContext();
//         |  fun_t fun;
//         |  fun_init(ctx, &fun);
//         |
//         |  ${hostCode.init}
//         |
//         |  int iterations = atoi(argv[1]);
//         |  for (int sample = 0; sample < iterations; sample++) {
//         |    ${hostCode.compute}
//         |  }
//         |  ${hostCode.finish}
//         |  fun_destroy(ctx, &fun);
//         |  destroyContext(ctx);
//         |  return EXIT_SUCCESS;
//         |}
//         |""".stripMargin
//    }
//
//
//    print("program: \n" + program)


  }


  // warning: test fails
  // java.lang.Exception: Don't know how to assign value of type <4>f32
  test("execute nbodyNoTuning") {

    // could not solve constraints
    val nbody: Expr = wrapOclRun(LocalSize(256, 1), GlobalSize(512, 1))(
      nbodyNVIDIAWithParams(256, 1))

    // could not solve constraints
    //    val nbody: Expr = nbodyNVIDIAKnownSizes.

    //    println("nbody: \n" + nbody)

    //    val code = gen.opencl.hosted.fromExpr(nbody)
    val code = gen.opencl.kernel.fromExpr(nbody)

    println("code: \n" + code)

    //    val code2 = gen.opencl.kernel.fromExpr(nbodyNVIDIAWithParams(256, 1))
    //    println("code2: \n" + code2)

    val codeHosted = gen.opencl.hosted("fun").fromExpr(nbody)
    println("codeHosted: \n" + codeHosted)
  }

  test("execute nbody") {

    val result = autotune.execution.execute(
      expression = nbodyNoTuning,
      hostCode = HostCode(init(512), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100,
      execution = Median,
    )

    println("result: " + result)
  }

  test("execute nbody tuning") {

    // todo fix error
    // <4>f32 types in C are not supported
    val nbody: Expr =
    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
      tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
        tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
          tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(nbodyNVIDIAWithParams(256, 1))
          ))))

    //    println("nbody2: " + nbody2)

    // could not solve constraints
    //    val nbody: Expr =
    //      tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
    //        tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
    //          tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
    //            tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
    //              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(nbodyTuning)
    //            ))))

    println("nbody: \n" + nbody)

    val params: Map[Nat, Nat] = Map(
      TuningParameter("vec") -> (4: Nat),
      TuningParameter("tileX") -> (256: Nat),
      TuningParameter("tileY") -> (1: Nat),
      TuningParameter("ls0") -> (256: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (512: Nat),
      TuningParameter("gs1") -> (1: Nat)
    )

    val nbodyReplaced = rise.core.substitute.natsInExpr(params, nbody)

    val result = autotune.execution.execute(
      expression = nbodyReplaced,
      hostCode = HostCode(init(512), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100,
      execution = Median
    )

    println("result: " + result)
  }

  test("search nbody") {

    println("initalisze")

    // could not solve constraints
    val nbody =
      tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
        tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
          tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
            tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(nbodyTuning)
            ))))

    println("initalized")

    val tuner = Tuner(
      hostCode = HostCode(init(1024), compute, finish),
      inputSizes = Seq(1024),
      samples = 20, // defined by config file
      name = "nbody",
      output = s"autotuning/nbody/nbody",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = None,
      hmConstraints = true,
      runtimeStatistic = Minimum,
      saveToFile = true
    )

    val tuningResult = autotune.search(tuner)(nbody)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }
}
