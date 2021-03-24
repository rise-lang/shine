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
              oclRun(LocalSize(1), GlobalSize(1))(
              input |> tileShiftInwards(tile)(mapWorkGroup(0)(
                slideVectors(vec) >> slide(3)(vec) >>
                  mapLocal(0)(weightsSeqVecUnroll(weights)) >>
                  asScalar
              ))
            ))))))

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
    val params = autotune.collectParameters(convolution)
    assert(params.find(_.name == "vec").get.range == RangeAdd(0, 32, 1))
    assert(params.find(_.name == "tile").get.range == RangeAdd(4, 32, 1))
    assert(params.size == 2)
  }

  test("collect constraints") {
    val e: Expr = convolution
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
    val e:Expr = convolution(32)

    val wrapped = wrapOclRun(e)(LocalSize(1), GlobalSize(1))
  }

  test("search"){
    val e:Expr = convolutionOcl(32)
    autotune.search(e)
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
      Buffer input = createBuffer(ctx, N * sizeof(int32_t), HOST_READ | HOST_WRITE | DEVICE_READ);
      Buffer output = createBuffer(ctx, N * sizeof(int32_t), HOST_READ | HOST_WRITE | DEVICE_WRITE);

      int32_t* in = hostBufferSync(ctx, input, N * sizeof(int32_t), HOST_WRITE);
      for (int i = 0; i < N; i++) {
        in[i] = 1;
      }

      foo(ctx, output, input, 4);

      int32_t* out = hostBufferSync(ctx, output, N * sizeof(int32_t), HOST_READ);

      for (int i = 0; i < N; i++) {
        if (out[i] != 4) {
          fprintf(stderr, "wrong output: %i\n", out[i]);
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
    util.ExecuteOpenCL(program, "zero_copy")
  }
}
