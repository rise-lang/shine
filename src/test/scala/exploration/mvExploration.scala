package exploration

import apps.gemv.ocl.gemvKeplerBest
import apps.gemv.{dot, scal}
import apps.separableConvolution2D
import apps.separableConvolution2D.mulT
import arithexpr.arithmetic.RangeMul
import elevate.core.{Failure, Strategy, Success}
import elevate.core.strategies.traversal.{allTopdown, bottomUp, topDown, tryAll}
import exploration.strategies.defaultStrategiesGPU
import rise.autotune.{HostCode, tuningParam, wrapOclRun}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._
import rise.elevate.rules.algorithmic.{fuseReduceMap, splitJoin}
import rise.elevate.rules.lowering.{addRequiredCopies, reduceOCL}
import rise.elevate.rules.traversal.default.RiseTraversable
import rise.elevate.{Rise, tunable}
import rise.openCL.DSL.{mapGlobal, toGlobal}
import rise.openCL.primitives.oclReduceSeq
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen


object mvExploration {
  // we can use implicit type parameters and type annotations to specify the function type of mult
  val mult = impl{ dt: DataType => fun(x => x._1 * x._2) :: ((dt x dt) ->: dt) }
  val add = fun(x => fun(y => x + y))
  val scal = impl { n: Nat =>
    fun(xs => fun(a =>
      map(fun(x => a * x))(xs)
    )) :: (ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
  }

  val dot: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduce(add)(lf32(0.0f))
  ))
  val dotSeq: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduce(add)(lf32(0.0f))
  ))


  // ocl functions
  val dotOcl: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> mapSeq(mulT) |> oclReduceSeq(AddressSpace.Global)(add)(lf32(0.0f))
  ))

  val scalOcl = impl { n: Nat =>
    fun(xs => fun(a =>
      mapSeq(fun(x => a * x))(xs)
    )) :: (ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
  }

//  val mvHighLevel = depFun((n: Nat, m: Nat) => fun(
//    (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32)
//  )((mat, xs) =>
//    mat |> map(fun(row =>
//      zip(row)(xs) |> map(mulT) |> reduce(add)(lf32(0.0f))
//    ))
//  ))

  val mvHighLevel = depFun((n: Nat, m: Nat) => fun(
    (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
      (m`.`f32)
  )((mat, xs, ys, alpha, beta) =>
    zip(map(fun(row => alpha * dot(row, xs)))(mat))(scal(ys, beta)) |>
      map(fun(x => x._1 + x._2))
  ))

  val mvOcl = depFun((n: Nat, m: Nat) => fun(
    (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
      (m`.`f32)
  )((mat, xs, ys, alpha, beta) =>
    zip(mapGlobal(0)(fun(row => alpha * dotOcl(row, xs)))(mat))(scalOcl(ys, beta)) |>
      mapSeq(fun(x => x._1 + x._2))
  ))

  println("mvHighLevel: " + mvHighLevel)

// mv hostocde
//  // scalastyle:off
//  val init: (Int, Int) => String = (N, M) => {
//    s"""
//       |const int N = ${N};
//       |const int M = ${M};
//       |
//       |srand(time(NULL));
//       |
//       |Buffer inputM = createBuffer(ctx, M * N * sizeof(float), HOST_WRITE | DEVICE_READ);
//       |Buffer inputV = createBuffer(ctx, N * sizeof(float), HOST_WRITE | DEVICE_READ);
//       |Buffer outputV = createBuffer(ctx, M * sizeof(float), HOST_READ | DEVICE_WRITE);
//       |
//       |float* inM = hostBufferSync(ctx, inputM, N * M * sizeof(float), HOST_WRITE);
//       |for (int i = 0; i < N * M ; i++) {
//       |  inM[i] = (float)(rand());
//       |}
//       |
//       |float* inV = hostBufferSync(ctx, inputV, N * sizeof(float), HOST_WRITE);
//       |for (int i = 0; i < N; i++) {
//       |  inV[i] = (float)(rand());
//       |}
//       |
//       |""".stripMargin
//  }
//
//  val compute =
//    s"""
//       |fun_run(ctx, &fun, outputV, M, N, inputM, inputV);
//       |""".stripMargin
//
//  val finish =
//    s"""
//       |// TODO: could check output here
//       |
//       |destroyBuffer(ctx, inputM);
//       |destroyBuffer(ctx, inputV);
//       |destroyBuffer(ctx, outputV);
//       |""".stripMargin
//  // scalastyle:on


  // gemv hostcode
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

  val lowered = defaultStrategiesGPU.lowering.apply(mvHighLevel)
  println("lowered: " + lowered)

  val mvNoTuning = wrapOclRun(LocalSize(32), GlobalSize(1024))(lowered.get)
  println("mvNoTuning: " + mvNoTuning)
  println("mvOcl: " + mvOcl)

  val codeOcl = gen.opencl.hosted("fun").fromExpr(mvOcl)

  println("codeOcl: " + codeOcl)

  val codeNoTuning = gen.opencl.hosted("fun").fromExpr(mvNoTuning)
  val codeNoTuningString = gen.opencl.hosted.asString(codeNoTuning)
  println("codeNoTuning: " + codeNoTuning)
  println("codeNoTuningString: " + codeNoTuningString)


  val lowered2 = defaultStrategiesGPU.lowering.apply(defaultStrategiesGPU.sjbu.apply(mvHighLevel).get)

  val mvNoTuning2 = wrapOclRun(LocalSize(32), GlobalSize(1024))(lowered.get)
  println("mvNoTuning2: " + mvNoTuning2)

  val codeNoTuning2 = gen.opencl.hosted("fun").fromExpr(mvNoTuning2)
  val codeNoTuningString2 = gen.opencl.hosted.asString(codeNoTuning2)
  println("codeNoTuning2: " + codeNoTuning2)
  println("codeNoTuningString2: " + codeNoTuningString2)


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


  def main(args: Array[String]): Unit = {
    // start exploration here

    // add strategies as arguments
//    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv.json", Some(HostCode(init(1024, 1024), compute, finish)))
  }
}
