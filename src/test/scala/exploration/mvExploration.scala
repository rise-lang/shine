package exploration

import apps.gemv.ocl.gemvKeplerBest
import apps.gemv.{dot, scal}
import apps.separableConvolution2D
import apps.separableConvolution2D.mulT
import arithexpr.arithmetic.RangeMul
import elevate.core.{Failure, Strategy, Success}
import elevate.core.strategies.traversal.{allTopdown, bottomUp, topDown, tryAll}
import exploration.strategies.defaultStrategiesGPU
import rise.autotune
import rise.autotune.{HostCode, Median, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._
import rise.elevate.rules.algorithmic.{fuseReduceMap, fuseReduceMap2, splitJoin}
import rise.elevate.rules.lowering.{addRequiredCopies, reduceOCL}
import rise.elevate.rules.traversal.default
import rise.elevate.rules.traversal.default.RiseTraversable
import rise.elevate.strategies.normalForm.DFNF
import rise.elevate.{Rise, tunable}
import rise.openCL.DSL.{mapGlobal, toGlobal}
import rise.openCL.primitives.oclReduceSeq
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen

//import elevate.core._
//import elevate.core.strategies.basic._
////import elevate.core.strategies.debug.peek
////import rise.core.IsClosedForm
//import elevate.core.strategies.debug.debug
//import elevate.core.strategies.traversal._
//import rise.core.DSL._
//import rise.core.primitives._
//import rise.core.types.DataType._
//import rise.elevate.rules.algorithmic._
//import rise.elevate.rules.lowering._
//import rise.elevate.rules.traversal._
//import rise.elevate.rules.traversal.default._
//import rise.elevate.strategies.algorithmic.reorder
//import rise.elevate.strategies.lowering._
//import rise.elevate.strategies.normalForm._
//import rise.elevate.strategies.predicate._
//import rise.elevate.strategies.tiling._
//import rise.elevate.strategies.traversal
import rise.elevate.strategies.traversal._

import _root_.util.gen

object mvExploration {

  // sub expressions
  val mult = impl { dt: DataType => fun(x => x._1 * x._2) :: ((dt x dt) ->: dt) }
  val add = fun(x => fun(y => x + y))
  val scal = impl { n: Nat =>
    fun(xs => fun(a =>
      map(fun(x => a * x))(xs)
    )) :: (ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
  }

  val dot: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduce(add)(lf32(0.0f))
  ))

  // lowered versions
  val dotSeq: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduce(add)(lf32(0.0f))
  ))

  val dotOcl: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> mapSeq(mulT) |> oclReduceSeq(AddressSpace.Global)(add)(lf32(0.0f))
  ))

  val scalOcl = impl { n: Nat =>
    fun(xs => fun(a =>
      mapSeq(fun(x => a * x))(xs)
    )) :: (ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
  }

  // main expressions
  val mvHighLevel = depFun((n: Nat, m: Nat) => fun(
    (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
  )((mat, xs) =>
    mat |> map(fun(row =>
      zip(row)(xs) |> map(mulT) |> reduce(add)(lf32(0.0f))
    ))
  ))

  val gemvHighLevel = depFun((n: Nat, m: Nat) => fun(
    (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32) ->: f32 ->: f32 ->:
      (m `.` f32)
  )((mat, xs, ys, alpha, beta) =>
    zip(map(fun(row => alpha * dot(row, xs)))(mat))(scal(ys, beta)) |>
      map(fun(x => x._1 + x._2))
  ))

  val mvOcl = depFun((n: Nat, m: Nat) => fun(
    (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32) ->: f32 ->: f32 ->:
      (m `.` f32)
  )((mat, xs, ys, alpha, beta) =>
    zip(mapSeq(fun(row => alpha * dotOcl(row, xs)))(mat))(scalOcl(ys, beta)) |>
      mapGlobal(0)(fun(x => x._1 + x._2))
  ))

  object mvHostCode {
    // scalastyle:off
    val init: (Int, Int) => String = (N, M) => {
      s"""
         |const int N = ${N};
         |const int M = ${M};
         |
         |srand(time(NULL));
         |
         |Buffer inputM = createBuffer(ctx, M * N * sizeof(float), HOST_WRITE | DEVICE_READ);
         |Buffer inputV = createBuffer(ctx, N * sizeof(float), HOST_WRITE | DEVICE_READ);
         |Buffer outputV = createBuffer(ctx, M * sizeof(float), HOST_READ | DEVICE_WRITE);
         |
         |float* inM = hostBufferSync(ctx, inputM, N * M * sizeof(float), HOST_WRITE);
         |for (int i = 0; i < N * M ; i++) {
         |  inM[i] = (float)(rand());
         |}
         |
         |float* inV = hostBufferSync(ctx, inputV, N * sizeof(float), HOST_WRITE);
         |for (int i = 0; i < N; i++) {
         |  inV[i] = (float)(rand());
         |}
         |
         |""".stripMargin
    }

    val compute =
      s"""
         |fun_run(ctx, &fun, outputV, M, N, inputM, inputV);
         |""".stripMargin

    val finish =
      s"""
         |// TODO: could check output here
         |
         |destroyBuffer(ctx, inputM);
         |destroyBuffer(ctx, inputV);
         |destroyBuffer(ctx, outputV);
         |""".stripMargin
    // scalastyle:on
  }

  object gemvHostCode {
    // scalastyle:off
  val init: (Int, Int) => String = (N, M) => {
    s"""
       |const int N = ${N};
       |const int M = ${M};
       |
       |srand(time(NULL));
       |
       |Buffer inputM = createBuffer(ctx, N * M * sizeof(float), HOST_WRITE | DEVICE_READ);
       |Buffer inputX = createBuffer(ctx, N * sizeof(float), HOST_READ | DEVICE_WRITE);
       |Buffer inputY = createBuffer(ctx, M * sizeof(float), HOST_READ | DEVICE_WRITE);
       |Buffer outputZ = createBuffer(ctx, N * sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |float* inM = hostBufferSync(ctx, inputM, N * M * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N * M; i++) {
       |  inM[i] = (float)(rand());
       |}
       |
       |float* inX = hostBufferSync(ctx, inputX, N * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N; i++) {
       |  inX[i] = (float)(rand());
       |}
       |
       |float* inY = hostBufferSync(ctx, inputY, M * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < M; i++) {
       |  inY[i] = (float)(rand());
       |}
       |
       |int alpha = (float)(rand());
       |int beta = (float)(rand());
       |
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, outputZ, M, N, inputM, inputX, inputY, alpha, beta);
       |""".stripMargin

  val finish =
    s"""
       |// TODO: could check output here
       |
       |destroyBuffer(ctx, inputM);
       |destroyBuffer(ctx, inputX);
       |destroyBuffer(ctx, inputY);
       |destroyBuffer(ctx, outputZ);
       |""".stripMargin
  // scalastyle:on
  }

//  val lowered = defaultStrategiesGPU.lowering.apply(mvHighLevel)
//  println("lowered: " + lowered)
//
//  val mvNoTuning = wrapOclRun(LocalSize(32), GlobalSize(1024))(lowered.get)
//  println("mvNoTuning: " + mvNoTuning)
//
//  val mvOclLowered = wrapOclRun(LocalSize(32), GlobalSize(1024))(mvOcl)
//  println("mvOclLowered: " + mvOclLowered)
//
//  val codeOcl = gen.opencl.hosted("fun").fromExpr(mvOclLowered)
//  println("codeOcl: " + codeOcl)
//
//  val codeNoTuning = gen.opencl.hosted("fun").fromExpr(mvNoTuning)
//  println("codeNoTuning: " + codeNoTuning)
//
//  val codeNoTuningString = gen.opencl.hosted.asString(codeNoTuning)
//  println("codeNoTuningString: " + codeNoTuningString)
//
//
//  val lowered2 = defaultStrategiesGPU.lowering.apply(defaultStrategiesGPU.sjbu.apply(mvHighLevel).get)
//
//  val mvNoTuning2 = wrapOclRun(LocalSize(32), GlobalSize(1024))(lowered.get)
//  println("mvNoTuning2: " + mvNoTuning2)
//
//  val codeNoTuning2 = gen.opencl.hosted("fun").fromExpr(mvNoTuning2)
//  val codeNoTuningString2 = gen.opencl.hosted.asString(codeNoTuning2)
//  println("codeNoTuning2: " + codeNoTuning2)
//  println("codeNoTuningString2: " + codeNoTuningString2)

  def checkExpression(e: Expr, hostCode: HostCode) = {
//    println("expression: \n" + e)

//    println("fused: \n" + fuseReduceMap2.apply(e))

    val lowered = defaultStrategiesGPU.lowering.apply(e)
//    println("lowered: \n" + lowered)

    val ocl = lowered match {
      case Success(p) =>
        println("Success")

        val ocl =
          tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
            tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
              tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
                tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
                  wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(lowered.get)
                ))))

//        val ocl = wrapOclRun(LocalSize(32), GlobalSize(1024))(lowered.get)
//        println("lowered ocl: " + ocl)
        ocl
      case Failure(s) =>
        println("Failure")
        val ocl = wrapOclRun(LocalSize(32), GlobalSize(1024))(e)
//        println("lowered ocl: " + ocl)
        ocl
    }

//    val code = gen.opencl.hosted("fun").fromExpr(ocl)
//    println("code: " + code)

//    val codeString = gen.opencl.hosted.asString(code)
//    println("code as string: " + codeString)

    val iterations = 10

    //    val tuner = Tuner()
    val tuner = Tuner(
      //      hostCode = HostCode(init(1024), compute, finish),
      hostCode = hostCode,
      inputSizes = Seq(1024, 1024),
      samples = iterations,
      name = "mv",
      output = "exploration/",
      timeouts = Timeouts(100000, 100000, 100000),
      executionIterations = 10,
      runtimeStatistic = Median,
      speedupFactor = 1.0,
      None,
      //      Some("/home/jo/development/rise-lang/shine/autotuning/scal/scal.json"),
      hmConstraints = true,
      saveToFile = false
    )

    val result = autotune.search(tuner)(ocl)

//    val result = autotune.execution.execute(
//      expression = ocl,
//      hostCode = hostCode,
//      timeouts = Timeouts(5000, 5000, 5000),
//      executionIterations = 100,
//      speedupFactor = 100,
//      execution = Median
//    )
    val best = autotune.getBest(result.samples)
//    println("result: " + result.runtime)
    println("best: " + best)
    println("\n")
  }

//  println("\n\n\nTest Rewrites")

  def testRewrite(rewrite: Strategy[Rise], lowering: Strategy[Rise]): Boolean = {
    println("highLevel: \n" + mvHighLevel)
    println("rewrite: " + rewrite)


    try{

      val rewritten = rewrite.apply(mvHighLevel)
      println("rewritten: \n" + rewritten.get)
      println("lowering: " + lowering)


    val lowered = rewritten match {
      case Success(p) => lowering.apply(rewritten.get)
      case Failure(s) => Failure(s)
    }

    println("lowered: \n" + lowered)

      lowered match {
        case Success(p) => true
        case Failure(s) => false
      }

    } catch {
      case e:Throwable =>
        println("e: " + e)
        false
    }
  }

  def rewrite(expr: Expr, strategy: Strategy[Rise]) = {

    println("expr: \n " + expr)

    val rewritten = strategy.apply(expr)

    println("rewritten: \n" + rewritten.get)

    rewritten.get
  }

//  val rewriteLayer1 = defaultStrategiesGPU.strategies.map(s => s.apply(mvHighLevel)).filter(e => e.isInstanceOf[Success]).map {
//    case Success(e) => e
//  }
//
//
//  // check, if all rewrites are valid
//  var i = 0
//  rewriteLayer1.foreach(rewrite => {
//    i = testRewrite(rewrite, defaultStrategiesGPU.lowering) match{
//      case true => i + 1
//      case false => i
//    }
//  })

//  println("true: " + i)
//  println("false: " + (defaultStrategiesGPU.strategies.size  - i))
//  object strategies {
//  }


//  val baseline: Strategy[Rise] = DFNF()(default.RiseTraversable) `;`
//    fuseReduceMap `@` topDown[Rise]

  val step0: Strategy[Rise] = fuseReduceMap `@` topDown[Rise]
//  val step1: Strategy[Rise] = tunable(splitJoin) `@` topDown[Rise]
  val step1: Strategy[Rise] = splitJoin(1024) `@` bottomUp[Rise]

  // reorderWithStride
//  val step2: Strategy[Rise] =

  // split join reduce
  val step2: Strategy[Rise] = tunable(splitJoin) `@` bottomUp[Rise]

  // fused stuff

  // normal forms?

    val strategy: Strategy[Rise] = tunable(splitJoin) `@` topDown[Rise]
//  val strategy: Strategy[Rise] = splitJoin(8) `@` topDown[Rise]






  def main(args: Array[String]): Unit = {

////    mvHighLevel
//
//    println("highLevel: " + mvHighLevel)
//
//    val sj = strategy.apply(mvHighLevel).get
////    val l1 = defaultStrategiesGPU.lowering.apply(sj).get
////    val l1OCL = wrapOclRun(LocalSize(32), GlobalSize(1024))(l1)
////
//    println("sj: \n" + sj)
////    println("l1: " + l1)
//
//
//    val sj2 = strategy.apply(sj).get
////    val l2 = defaultStrategiesGPU.lowering.apply(sj2)
//    println("sj2: \n" + sj2)
////    println("l2: " + l2)
//
//    val sj3 = strategy.apply(sj2).get
////    val l3 = defaultStrategiesGPU.lowering.apply(sj3)
////    println("sj3: " + sj3)
////    println("l3: " + l3)
//    val sj4 = strategy.apply(sj3).get
//    val sj5 = strategy.apply(sj4).get
//    val sj6 = strategy.apply(sj5).get
//
//    val hostcode = HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)

//    checkExpression(sj, hostcode)
//    checkExpression(sj2, hostcode)
//    checkExpression(sj3, hostcode)
//    checkExpression(sj4, hostcode)
//    checkExpression(sj5, hostcode)
//    checkExpression(sj6, hostcode)

//
//    val string =
//      "hello\n" +
//        "my name is \n" +
//        "johanns \n"
//
//        println("string: \n" + string)
//
//    val reverse = string.split("\n")
//      .toSeq
//      .map(_.trim)
//      .filter(_ != "").reverse.mkString("\n")
//
//    println("reverse: \n" + reverse)



    // test hash


//    val hash = elevate.heuristic_search.util.hashProgram(mvHighLevel)
//    println("hash: " + hash)
//    val hash2 = elevate.heuristic_search.util.hashProgram(mvHighLevel)
//    println("hash2: " + hash2)


//    // mvFused
//    val mvFusedRewrite = step0
//    rewrite(mvHighLevel, mvFusedRewrite)
//    // add lowering
//
//    // mvFusedSplit
//    val mvFusedSplitRewrite = step0 `;` step1
//    rewrite(mvHighLevel, mvFusedSplitRewrite)
    // add lowering

    // mvBlastN
//    val mvBlastNRewrite =

    // mvBlastT
//    val mvBlastTRewrite =

    // mvKeplerBest
//    val mvKeplerBestRewrite =

    // mvFusedAMD
//    val mvFusedAMDRewrite =



    // check expressions
//    checkExpression(mvHighLevel, HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish))
//    checkExpression(gemvHighLevel, HostCode(gemvHostCode.init(1024, 1024), gemvHostCode.compute, gemvHostCode.finish))
//    checkExpression(mvOcl, HostCode(gemvHostCode.init(1024, 1024), gemvHostCode.compute, gemvHostCode.finish))

    // start exploration here

    // add strategies as arguments
//    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv.json", Some(HostCode(init(1024, 1024), compute, finish)))
//        riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies2, "exploration/configuration/mv.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
//    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv_exhaustive.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv_tuner.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
//    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv_debug.json", None)
//    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))

//    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies2, "exploration/configuration/mv.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))

    //    val e1 = rewrite(mvHighLevel, step0)
//    val e2 = rewrite(e1, step1)
//    val e3 = rewrite(e2, step2)

//    println("mvFused: \n" + apps.mv.ocl.mvFused)
//    println("mvFused: \n" + apps.mv.ocl.mvFusedSplit.toExpr)
    }
    //    println("mvFused: \n" + apps.mv.ocl.mvFusedAMD)

}
