package apps.autotuning

import shine.DPIA.Types.ExpType
import shine.OpenCL.{GlobalSize, LocalSize}
import rise.core._
import rise.core.types._
import rise.core.DSL._
import rise.core.primitives._
import Type._
import HighLevelConstructs.reorderWithStride
import rise.autotune
import rise.autotune.{HostCode, Median, Timeouts, tuningParam, wrapOclRun}
import util.{SyntaxChecker, gen}
import rise.elevate.rules.traversal.default._
import rise.openCL.DSL.{mapLocal, mapWorkGroup, toLocalFun}
import rise.openCL.primitives.{oclIterate, oclReduceSeq}
import shine.OpenCL.KernelExecutor.KernelNoSizes.fromKernelModule
import util.gen.c.function


import apps.separableConvolution2D.mulT
import rise.core.DSL.HighLevelConstructs.reorderWithStride
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{let => _, _}
import rise.core.types.DataType._
import rise.core.types._

import scala.util.Random


import apps.mv.ocl._
import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}


import apps.mm.mmNVIDIAWithParams
import arithexpr.arithmetic.{RangeAdd, RangeMul}
import rise.core._
import rise.core.types._
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.autotune
import shine.OpenCL.{GlobalSize, LocalSize}
import rise.autotune._
import apps.autotuning._

import scala.language.postfixOps
import scala.sys.process._

//
class asumTuning extends test_util.Tests {
  //
  //

  val inputSize: Int = 2 << 12

  def inputT(n: Nat) = ArrayType(n, f32)

  val abs =
    depFun((t: DataType) => foreignFun("my_abs", Seq("y"), "{ return fabs(y); }", t ->: t))
  val fabs = abs(f32)
  val add = fun(x => fun(a => x + a))

  val high_level = depFun((n: Nat) =>
    fun(inputT(n))(input => input |> map(fabs) |> reduceSeq(add)(lf32(0.0f)))
  )
  //
  //  val abs =
  //    depFun((t: DataType) => foreignFun("my_abs", Seq("y"), "{ return fabs(y); }", t ->: t))
  //  val fabs = abs(f32)
  //  val add = fun(x => fun(a => x + a))
  //
  //  val high_level = depFun((n: Nat) =>
  //    fun(inputT(n))(input => input |> map(fabs) |> reduceSeq(add)(lf32(0.0f)))
  //  )
  //
  //  test("High level asum type inference works") {
  //    val typed = high_level.toExpr
  //
  //    val N = typed.t.asInstanceOf[NatDepFunType[_ <: Type, _ <: Kind.Identifier]].x
  //    assertResult(DepFunType(NatKind, N, FunType(inputT(N), f32))) {
  //      typed.t
  //    }
  //  }
  //
  //
  //  //
  //  // /tmp/code-4521009941088777169.c:24:67: error: use of undeclared identifier 'n329'
  //  // const KernelArg args[6] = (const KernelArg[6]){KARG(b0), KARG(n329), KARG(n326), KARG(n328), KARG(n327), KARG(b5)};
  //

  val nvidiaDerived1: ToBeTyped[Expr] = {
    tuningParam("sp0", RangeMul(1, inputSize, 2), (sp0: Nat) =>
      tuningParam("sp1", RangeMul(1, inputSize, 2), (sp1: Nat) =>
        tuningParam("stride", RangeMul(1, inputSize, 2), (stride: Nat) =>
          depFun((n: Nat) =>
            fun(inputT(n))(input =>
              input |>
                split(sp0) |>
                mapWorkGroup(
                  reorderWithStride(stride) >>
                    split(sp1) >>
                    mapLocal(
                      oclReduceSeq(AddressSpace.Private)(
                        fun(a => fun(x => abs(f32)(x) + a))
                      )(lf32(0.0f))
                    )
                ) |> join
            )
          ))))
  }


  // scalastyle:off
  val init: (Int) => String = (N) => {
    s"""
       |  const int N = ${N};
       |  srand(time(NULL));
       |  Buffer input = createBuffer(ctx, N * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer output = createBuffer(ctx, 1 * sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |  float* in = hostBufferSync(ctx, input, N * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < N ; i++) {
       |    in[i] = (float)(rand() % 100);
       |  }
       |
       |  deviceBufferSync(ctx, input, N * sizeof(float), DEVICE_READ);
       |""".stripMargin
  }
  val compute =
    s"""
       |    fun_init_run(ctx, output, N, input);
       |""".stripMargin

  val finish =
    s"""
       |  // could add error checking
       |  destroyBuffer(ctx, input);
       |  destroyBuffer(ctx, output);
       |""".stripMargin
  // scalastyle:on



  test("print asum") {

    println("inputSize: " + inputSize)

    println("nvidia derived: \n" + nvidiaDerived1)
    //
    //     opencl
    val asum: Expr = {
      tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
        tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
          wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(nvidiaDerived1)))
    }

    println("asum: \n" + asum)

    // generate code
    val params: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (128: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("sp0") -> ((2048 * 128): Nat),
      TuningParameter("sp1") -> (2048: Nat),
      TuningParameter("stride") -> (128: Nat),
    )

    val eSub = rise.core.substitute.natsInExpr(params, asum)
    val eSub2 = rise.core.substitute.natsInExpr(params, nvidiaDerived1)

    // generate code
    val kernel = util.gen.opencl.kernel.asStringFromExpr(eSub2)
    println("kernel: \n" + kernel)

    val tp_params = autotune.constraints.collectParameters(asum)
    val constraints = autotune.constraints.collectConstraints(asum, tp_params)

    println("Params: ")
    tp_params.foreach(elem => println(s"""${elem.name} - ${elem.range}"""))
    println("Constraints: ")
    constraints.foreach(println)
    //
    //    val result = autotune.execution.execute(
    //      expression = eSub,
    //      hostCode = HostCode(init(inputSize), compute, finish),
    //      timeouts = Timeouts(5000, 5000, 5000),
    //      executionIterations = 1000,
    //      speedupFactor = 1000,
    //      execution = Minimum
    //    )
    //    println("result: " + result.runtime)


    val tuner = Tuner(
      hostCode = HostCode(init(inputSize), compute, finish),
      inputSizes = Seq(inputSize),
      hmConstraints = true,
      samples = 10,
      saveToFile = true
    )

    //    val configFile = autotune.configFileGeneration.generateJSON(tp_params, constraints, tuner)
    //    println("configFile: \n" + configFile)


    val tuning_result = autotune.search(tuner)(asum)

  }


  // what depFun(_, ... is for?
  //    val amdNvidiaDerived2 =
  //    depFun((n: Nat) =>
  //      //    tuningParam("n", (n: Nat) =>
  //      tuningParam("sp0", (sp0: Nat) =>
  //        tuningParam("sp1", (sp1: Nat) =>
  //          tuningParam("sp2", (sp2: Nat) =>
  //            tuningParam("iterate", (iterate: Nat) =>
  //              fun(inputT(n))(input =>
  //                input |>
  //                  split(sp0) |>
  //                  mapWorkGroup(
  //                    split(sp1) >>
  //                      toLocalFun(
  //                        mapLocal(oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f)))
  //                      ) >>
  //                      toLocalFun(
  //                        oclIterate(AddressSpace.Local)(iterate)(
  //                          depFun((_: Nat) =>
  //                            split(sp2) >> mapLocal(
  //                              oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))
  //                            )
  //                          )
  //                        )
  //                      ) >> mapLocal(fun(x => x))
  //                  ) |> join
  //              )
  //            ))))
  //    )
  //
  //
  //  val asum2: Expr =
  //    tuningParam("ls0", (ls0: Nat) =>
  //      tuningParam("gs0", (gs0: Nat) =>
  //        wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(amdNvidiaDerived2)))
  //
  //  val asum: Expr =
  //    tuningParam("ls0", (ls0: Nat) =>
  //      tuningParam("gs0", (gs0: Nat) =>
  //        wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(nvidiaDerived1)))
  //
  //  // scalastyle:off
  //  val init: (Int) => String = (N) => {
  //    s"""
  //       |  const int N = ${N};
  //       |  srand(time(NULL));
  //       |  Buffer input = createBuffer(ctx, N * sizeof(float), HOST_WRITE | DEVICE_READ);
  //       |  Buffer output = createBuffer(ctx, 1 * sizeof(float), HOST_READ | DEVICE_WRITE);
  //       |
  //       |  float* in = hostBufferSync(ctx, features, N * sizeof(float), HOST_WRITE);
  //       |  for (int i = 0; i < N ; i++) {
  //       |    in[i] = (float)(rand() % 100);
  //       |  }
  //       |
  //       |  deviceBufferSync(ctx, features, N * sizeof(float), DEVICE_READ);
  //       |""".stripMargin
  //  }
  //  val compute =
  //    s"""
  //       |    fun_init_run(ctx, output, input);
  //       |""".stripMargin
  //
  //  val finish =
  //    s"""
  //       |  // could add error checking
  //       |  destroyBuffer(ctx, input);
  //       |  destroyBuffer(ctx, output);
  //       |""".stripMargin
  //  // scalastyle:on
  //
  //  test("get constraints asum") {
  //    val parameters = rise.autotune.constraints.collectParameters(asum)
  //    val constraints = rise.autotune.constraints.collectConstraints(asum, parameters)
  //
  //    println("parameters: ")
  //    parameters.foreach(println)
  //
  //    println("\nconstraints: ")
  //    constraints.foreach(println)
  //  }
  //
  //  test("execute asum") {
  //
  //    println("asum: \n" + asum)
  //
  //    val parameters = rise.autotune.constraints.collectParameters(asum)
  //    val constraints = rise.autotune.constraints.collectConstraints(asum, parameters)
  //
  //    val params = Map(
  //      TuningParameter("n") -> (8192: Nat),
  //      TuningParameter("sp0") -> (8192: Nat),
  //      TuningParameter("sp1") -> (128: Nat),
  //      //      TuningParameter("sp2") -> (2: Nat),
  //      //      TuningParameter("iterate") -> (6: Nat),
  //      TuningParameter("ls0") -> (128: Nat),
  //      TuningParameter("gs0") -> (8192: Nat)
  //    )
  //
  //    val asum_replaced = rise.core.substitute.natsInExpr(params, asum)
  //    println("asum_replaced: \n" + asum_replaced)
  //
  //    val check = rise.autotune.constraints.checkConstraints(constraints, params)
  //    println("constraints checking: " + check)
  //
  //    val result = autotune.execution.execute(
  //      expression = asum_replaced,
  //      hostCode = HostCode(init(8192), compute, finish),
  //      timeouts = Timeouts(5000, 5000, 5000),
  //      executionIterations = 10,
  //      speedupFactor = 100,
  //      execution = Median
  //    )
  //
  //    println("result: " + result)
  //
  //  }
  //
  //
}


import shine.DPIA.Types.ExpType
import shine.OpenCL.{GlobalSize, LocalSize}
import rise.core._
import rise.core.types._
import rise.core.DSL._
import rise.core.primitives._
import Type._
import HighLevelConstructs.reorderWithStride
import rise.autotune
import rise.autotune.{HostCode, Median, Timeouts, tuningParam, wrapOclRun}
import util.{SyntaxChecker, gen}
import rise.elevate.rules.traversal.default._
import rise.openCL.DSL.{mapLocal, mapWorkGroup, toLocalFun}
import rise.openCL.primitives.{oclIterate, oclReduceSeq}
import shine.OpenCL.KernelExecutor.KernelNoSizes.fromKernelModule
import util.gen.c.function

import scala.util.Random
//
//
//class asumTuning extends test_util.Tests {
//
//
//  def inputT(n: Nat) = ArrayType(n, f32)
//
//  val abs =
//    depFun((t: DataType) => foreignFun("my_abs", Seq("y"), "{ return fabs(y); }", t ->: t))
//  val fabs = abs(f32)
//  val add = fun(x => fun(a => x + a))
//
//  val high_level = depFun((n: Nat) =>
//    fun(inputT(n))(input => input |> map(fabs) |> reduceSeq(add)(lf32(0.0f)))
//  )
//
//  test("High level asum type inference works") {
//    val typed = high_level.toExpr
//
//    val N = typed.t.asInstanceOf[NatDepFunType[_ <: Type, _ <: Kind.Identifier]].x
//    assertResult(DepFunType(NatKind, N, FunType(inputT(N), f32))) {
//      typed.t
//    }
//  }
//
//
//  //
//  // /tmp/code-4521009941088777169.c:24:67: error: use of undeclared identifier 'n329'
//  // const KernelArg args[6] = (const KernelArg[6]){KARG(b0), KARG(n329), KARG(n326), KARG(n328), KARG(n327), KARG(b5)};
//
//  val nvidiaDerived1 =
////    depFun((n: Nat) =>
//          tuningParam("n", (n: Nat) =>
//    tuningParam("sp0", (sp0: Nat) =>
//      tuningParam("sp1", (sp1: Nat) =>
//        fun(inputT(n))(input =>
//          input |>
//            split(sp0) |>
//            mapWorkGroup(
//              reorderWithStride(128) >>
//                split(sp1) >>
//                mapLocal(
//                  oclReduceSeq(AddressSpace.Private)(
//                    fun(a => fun(x => abs(f32)(x) + a))
//                  )(lf32(0.0f))
//                )
//            ) |> join
//        )
//      )))
//
//
//  // what depFun(_, ... is for?
//  val amdNvidiaDerived2 =
//    depFun((n: Nat) =>
////    tuningParam("n", (n: Nat) =>
//    tuningParam("sp0", (sp0: Nat) =>
//      tuningParam("sp1", (sp1: Nat) =>
//        tuningParam("sp2", (sp2: Nat) =>
//          tuningParam("iterate", (iterate: Nat) =>
//            fun(inputT(n))(input =>
//              input |>
//                split(sp0) |>
//                mapWorkGroup(
//                  split(sp1) >>
//                    toLocalFun(
//                      mapLocal(oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f)))
//                    ) >>
//                    toLocalFun(
//                      oclIterate(AddressSpace.Local)(iterate)(
//                        depFun((_: Nat) =>
//                          split(sp2) >> mapLocal(
//                            oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))
//                          )
//                        )
//                      )
//                    ) >> mapLocal(fun(x => x))
//                ) |> join
//            )
//          ))))
//  )
//
//
//  val asum2: Expr =
//    tuningParam("ls0", (ls0: Nat) =>
//      tuningParam("gs0", (gs0: Nat) =>
//        wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(amdNvidiaDerived2)))
//
//  val asum: Expr =
//    tuningParam("ls0", (ls0: Nat) =>
//      tuningParam("gs0", (gs0: Nat) =>
//        wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(nvidiaDerived1)))
//
//  // scalastyle:off
//  val init: (Int) => String = (N) => {
//    s"""
//       |  const int N = ${N};
//       |  srand(time(NULL));
//       |  Buffer input = createBuffer(ctx, N * sizeof(float), HOST_WRITE | DEVICE_READ);
//       |  Buffer output = createBuffer(ctx, 1 * sizeof(float), HOST_READ | DEVICE_WRITE);
//       |
//       |  float* in = hostBufferSync(ctx, features, N * sizeof(float), HOST_WRITE);
//       |  for (int i = 0; i < N ; i++) {
//       |    in[i] = (float)(rand() % 100);
//       |  }
//       |
//       |  deviceBufferSync(ctx, features, N * sizeof(float), DEVICE_READ);
//       |""".stripMargin
//  }
//  val compute =
//    s"""
//       |    fun_init_run(ctx, output, input);
//       |""".stripMargin
//
//  val finish =
//    s"""
//       |  // could add error checking
//       |  destroyBuffer(ctx, input);
//       |  destroyBuffer(ctx, output);
//       |""".stripMargin
//  // scalastyle:on
//
//  test("get constraints asum"){
//    val parameters = rise.autotune.constraints.collectParameters(asum)
//    val constraints = rise.autotune.constraints.collectConstraints(asum, parameters)
//
//    println("parameters: ")
//    parameters.foreach(println)
//
//    println("\nconstraints: ")
//    constraints.foreach(println)
//  }
//
//  test("execute asum"){
//
//    println("asum: \n" + asum)
//
//    val parameters = rise.autotune.constraints.collectParameters(asum)
//    val constraints = rise.autotune.constraints.collectConstraints(asum, parameters)
//
//    val params = Map(
//      TuningParameter("n") -> (8192: Nat),
//      TuningParameter("sp0") -> (8192: Nat),
//      TuningParameter("sp1") -> (128: Nat),
////      TuningParameter("sp2") -> (2: Nat),
////      TuningParameter("iterate") -> (6: Nat),
//      TuningParameter("ls0") -> (128: Nat),
//      TuningParameter("gs0") -> (8192: Nat)
//    )
//
//    val asum_replaced = rise.core.substitute.natsInExpr(params, asum)
//    println("asum_replaced: \n" + asum_replaced)
//
//    val check = rise.autotune.constraints.checkConstraints(constraints, params)
//    println("constraints checking: " + check)
//
//    val result = autotune.execution.execute(
//      expression = asum_replaced,
//      hostCode = HostCode(init(8192), compute, finish),
//      timeouts = Timeouts(5000, 5000, 5000),
//      executionIterations = 10,
//      speedupFactor = 100,
//      execution = Median
//    )
//
//    println("result: " + result)
//
//  }
//
//
//}