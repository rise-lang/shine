package rise

import arithexpr.arithmetic.{NamedVar, PosInf, RangeAdd, RangeMul, RangeUnknown}
import rise.core._
import rise.core.types._
import rise.core.primitives._
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.DSL.HighLevelConstructs.{slideVectors, tileShiftInwards}
import rise.openCL.DSL._
import rise.autotune.{collectConstraints, tuningParam, wrapOclRun}
import apps.separableConvolution2D.weightsSeqVecUnroll
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen
//import shine.DPIA.Types.TypeCheck
//import util.gen


class autotuning extends test_util.Tests {
  val convolution: ToBeTyped[Expr] =
  // tileShiftInwards should constrain n >= tile
  // slideVectors and slide should constrain tile % vec = 0
    tuningParam("vec", RangeAdd(0, 32, 1), (vec: Nat) =>
      tuningParam("tile", RangeAdd(4, 32, 1), (tile: Nat) =>
        depFun(RangeAdd(1, PosInf, vec), (n: Nat) =>
          fun(3`.`f32)(weights =>
            fun(((n+2)`.`f32) ->: (n`.`f32))(input =>
              input |> tileShiftInwards(tile)(mapWorkGroup(0)(
                slideVectors(vec) >> slide(3)(vec) >>
                  mapLocal(0)(weightsSeqVecUnroll(weights)) >>
                  asScalar
              ))
            )))))

  val convolutionOcl: ToBeTyped[Expr] =
  // tileShiftInwards should constrain n >= tile
  // slideVectors and slide should constrain tile % vec = 0
    tuningParam("vec", RangeAdd(0, 32, 1), (vec: Nat) =>
      tuningParam("tile", RangeAdd(4, 32, 1), (tile: Nat) =>
        depFun(RangeAdd(1, PosInf, vec), (n: Nat) =>
          fun(3`.`f32)(weights =>
            fun(((n+2)`.`f32) ->: (n`.`f32))(input =>
              oclRun(LocalSize(1), GlobalSize(32))(
              input |> tileShiftInwards(tile)(mapWorkGroup(0)(
                slideVectors(vec) >> slide(3)(vec) >>
                  mapLocal(0)(weightsSeqVecUnroll(weights)) >>
                  asScalar
              ))
            ))))))

  val convolutionOcl2: Expr =
    tuningParam("ls0", (ls0: Nat) => tuningParam("ls1", (ls1: Nat) =>
    tuningParam("gs0", (gs0: Nat) => tuningParam("gs1", (gs1: Nat) =>
      wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(convolution)
    ))))

  val scalSJ: ToBeTyped[Expr] =
    depFun((n: Nat) => fun(n`.`f32)(input => fun(f32)(alpha =>
      oclRun(LocalSize(1), GlobalSize(32))(
      input |> split(4) |> mapGlobal(0)(fun(x => alpha * x)) |> join)
    )))

  val scalOcl: ToBeTyped[Expr] =
    depFun((n: Nat) => fun(n`.`f32)(input => fun(f32)(alpha =>
      oclRun(LocalSize(1), GlobalSize(32))(
        input |> mapGlobal(0)(fun(x => alpha * x)))
    )))

  val scal: ToBeTyped[Expr] =
    depFun((n: Nat) => fun(n`.`f32)(input => fun(f32)(alpha =>
        input |> mapGlobal(0)(fun(x => alpha * x)))
    ))

  test("collect parameters") {
    val params = autotune.collectParameters(convolutionOcl2)
    assert(params.find(_.name == "vec").get.range == RangeAdd(0, 32, 1))
    assert(params.find(_.name == "tile").get.range == RangeAdd(4, 32, 1))
    assert(params.find(_.name == "ls0").get.range == RangeUnknown)
    assert(params.find(_.name == "ls1").get.range == RangeUnknown)
    assert(params.find(_.name == "gs0").get.range == RangeUnknown)
    assert(params.find(_.name == "gs1").get.range == RangeUnknown)
    assert(params.size == 6)
  }

  test("collect constraints") {
    val e: Expr = convolutionOcl2
    autotune.collectConstraints(e, autotune.collectParameters(e)).foreach(println)
  }

  test("substitute parameters") {
    val e: Expr = convolution(32)
    val constraints = autotune.collectConstraints(e, autotune.collectParameters(e))
    println("constraints: \n" + constraints)

    val badParameters1 = Map(
      NatIdentifier("vec", isExplicit = true) -> (5: Nat),
      NatIdentifier("tile", isExplicit = true) -> (15: Nat)
    )
    assert(!autotune.checkConstraints(constraints, badParameters1))

    val badParameters2 = Map(
      NatIdentifier("vec", isExplicit = true) -> (4: Nat),
      NatIdentifier("tile", isExplicit = true) -> (13: Nat)
    )
    assert(!autotune.checkConstraints(constraints, badParameters2))

    /* FIXME: there is no `n >= tile` constraint collected
    val badParameters3 = Map(
      NatIdentifier("vec", isExplicit = true) -> (8: Nat),
      NatIdentifier("tile", isExplicit = true) -> (64: Nat)
    )
    assert(!autotune.checkConstraints(constraints, badParameters3))
    */

    val goodParameters = Map(
      NatIdentifier("vec", isExplicit = true) -> (4: Nat),
      NatIdentifier("tile", isExplicit = true) -> (16: Nat)
    )
    assert(autotune.checkConstraints(constraints, goodParameters))
    rise.core.substitute.natsInExpr(goodParameters, e)
  }

  test("generateJSON"){
    val json = autotune.generateJSON(autotune.collectParameters(convolution))

    // create gold
    // check against gold

    println("json: \n" + json)
  }

  test("wrapOclRun"){
    val wrapped = wrapOclRun(LocalSize(1), GlobalSize(1))(convolution)
    assert(convolutionOcl.toExpr == wrapped)

    val e = (wrapped: ToBeTyped[Expr])(32)
    assert(convolutionOcl(32).toExpr == e.toExpr)

    val tuningResult = autotune.search(e)

    val bestSample = autotune.getBest(tuningResult)
    println("bestSample: \n" + bestSample)
  }

  test("search"){
    val e:Expr = convolutionOcl(32)
    val tuningResult = autotune.search(e)

    val bestSample = autotune.getBest(tuningResult)
    println("bestSample: \n" + bestSample)
  }

  test("execute convolution"){
    val goodParameters = Map(
      NatIdentifier("vec", isExplicit = true) -> (4: Nat),
      NatIdentifier("tile", isExplicit = true) -> (16: Nat)
    )

    val e:Expr = convolutionOcl(32)
    val e2 = rise.core.substitute.natsInExpr(goodParameters, e)

    val result = autotune.execute(e2)
    println("result: " + result)
  }

  test("execute scal"){
    val e:Expr = scalOcl(32)

    val main = """
    const int N = 32;
    int main(int argc, char** argv) {
      Context ctx = createDefaultContext();
      Buffer input = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
      Buffer output = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);

      float* in = hostBufferSync(ctx, input, N * sizeof(float), HOST_WRITE);
      for (int i = 0; i < N; i++) {
        in[i] = 1;
      }

      foo(ctx, output, input, 4);

      float* out = hostBufferSync(ctx, output, N * sizeof(float), HOST_READ);

      for (int i = 0; i < N; i++) {
        if (out[i] != 4) {
          fprintf(stderr, "wrong output: %f\n", out[i]);
          exit(EXIT_FAILURE);
        }
      }

      destroyBuffer(ctx, input);
      destroyBuffer(ctx, output);
      destroyContext(ctx);
      return EXIT_SUCCESS;
    }
    """

    val m = gen.opencl.hosted.fromExpr(e)
    val program = shine.OpenCL.Module.translateToString(m) + main
    println("program: \n" + program)
    val runtime = util.ExecuteOpenCL.executeWithRuntime(program, "zero_copy")
    println("result: \n" + runtime)
  }

  test("test xml parsing") {
    val xmlString = """
<trace date="2021-03-30 18:04:26" profiler_version="0.1.0" ocl_version="1.2">
  <device name="GeForce RTX 2070" id="1"/>
  <queue properties="CL_QUEUE_PROFILING_ENABLE" device_id="1" id="1"/>
  <program build_options="-cl-fast-relaxed-math -Werror -cl-std=CL1.2" id="1"/>
  <kernel name="k0" program_id="1" id="1"/>
  <kernel_instance kernel_id="1" id="1" unique_id="1" command_queue_id="1">
    <event forced="true" queued="1617120266695090400" submit="1617120266695092832" start="1617120266695097344" end="1617120266695107456"/>
    <offset_range/>
    <global_range dim="3" x="1" y="1" z="1"/>
    <local_range dim="3" x="1" y="1" z="1"/>
  </kernel_instance>
  <mem_object type="Buffer" flag="CL_MEM_WRITE_ONLY|CL_MEM_ALLOC_HOST_PTR" size="128" id="2"/>
  <mem_object type="Buffer" flag="CL_MEM_READ_ONLY|CL_MEM_ALLOC_HOST_PTR" size="128" id="1"/>
</trace>
    """
    assert(util.ExecuteOpenCL.getRuntimeFromClap(xmlString).value.toFloat == 0.010112f)
  }

  test("text xml parsing with corrupted xml string"){
    val corruptedXmlString = """<trace date="2021-04-01 18:42:51" profiler_version="0.1.0" ocl_version="1.2"/>
<trace date="2021-04-01 18:42:51" profiler_version="0.1.0" ocl_version="1.2">
  <device name="pthread-Intel(R) Core(TM) i5-8265U CPU @ 1.60GHz" id="1"/>
  <queue properties="CL_QUEUE_PROFILING_ENABLE" device_id="1" id="1"/>
  <program build_options="-cl-fast-relaxed-math -Werror -cl-std=CL1.2" id="1"/>
  <kernel name="k0" program_id="1" id="1"/>
  <kernel_instance kernel_id="1" id="1" unique_id="1" command_queue_id="1">
    <event forced="true" queued="42573479270519" submit="42573479270669" start="42573583785589" end="42573583944408"/>
    <offset_range/>
    <global_range dim="3" x="32" y="1" z="1"/>
    <local_range dim="3" x="1" y="1" z="1"/>
  </kernel_instance>
  <mem_object type="Buffer" flag="CL_MEM_WRITE_ONLY|CL_MEM_ALLOC_HOST_PTR" size="128" id="2"/>
  <mem_object type="Buffer" flag="CL_MEM_READ_ONLY|CL_MEM_ALLOC_HOST_PTR" size="128" id="1"/>
</trace>

    """
    assert(util.ExecuteOpenCL.getRuntimeFromClap(corruptedXmlString).value.toFloat == 0.158819f)
  }
}
