package exploration.runner

import arithexpr.arithmetic.RangeMul
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.heuristic_search.Runner
import elevate.heuristic_search.util.Solution
import rise.autotune.{HostCode, Median, Timeouts, Tuner, applyBest, getBest, search, tuningParam, wrapOclRun}
import rise.core.Expr
import rise.core.types.Nat
import rise.elevate.Rise
import rise.eqsat.Rewrite
import shine.OpenCL.{GlobalSize, LocalSize}

case class AutoTuningExecutor(lowering: Strategy[Rise],
                       goldExpression: Rise,
                       iterations: Int,
                       inputSize: Int,
                       threshold: Double,
                       output: String) extends Runner[Rise] {

  // hard coded hostcode for convolution
  val init: (Int) => String = (N) => {
    s"""
       |const int N = ${N};
       |srand(time(NULL));
       |Buffer matrix = createBuffer(ctx, N * N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer weights = createBuffer(ctx, 17 * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer output = createBuffer(ctx, N * N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
       |
       |float* m = hostBufferSync(ctx, matrix, N * N * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N * N; i++) {
       |  m[i] = (float)(rand())/(float)(RAND_MAX) * 10.0f;
       |}
       |
       |float* w = hostBufferSync(ctx, weights, 17 * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < 17; i++) {
       |  w[i] = (float)(rand())/(float)(RAND_MAX);
       |}
       |
       |// synchronize before entering timed section
       |deviceBufferSync(ctx, matrix, N * N * sizeof(float), DEVICE_READ);
       |deviceBufferSync(ctx, weights, 17 * sizeof(float), DEVICE_READ);
       |deviceBufferSync(ctx, output, N * N * sizeof(float), DEVICE_WRITE);
       |       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, output, N, matrix, weights);
       |""".stripMargin

  val finish =
    s"""
       |// TODO: could check output here
       |
       |destroyBuffer(ctx, matrix);
       |destroyBuffer(ctx, weights);
       |destroyBuffer(ctx, output);
       |""".stripMargin


  def execute(solution: Solution[Rise]):(Rise,Option[Double]) = {

    println("solution: " + solution)

    // create tuner
    val tuner = Tuner(
      hostCode = HostCode(init(1024), compute, finish),
      inputSizes = Seq(1024),
      samples = 100,
      name = "convolution",
      output = "autotuning/convolution",
      timeouts = Timeouts(1000, 1000, 1000),
      executionIterations = 10,
      speedupFactor = 100,
      None,
      hierarchicalHM = true,
      runtimeStatistic = Median
    )

    // lower expression
    val e:RewriteResult[Rise] = lowering.apply(solution.expression)


    // check, if lowering was successful
    e match{
      case Success(p) => {
        println("lowered: " + e.get)
        val lowered:Expr = e.get


        // now wrap ocl
        val eTuning: Expr =
          tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
            tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
              tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
                tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
                  wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(lowered)
                ))))
//
//        // now wrap ocl
//        val eTuning: Expr =
//          tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
//            tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
//              tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
//                tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
//                  wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(lowered)
//                ))))

        // run tuning
        val result = search(tuner)(eTuning)

        val best = getBest(result.samples)
        println("best: " + best)

        // report runtime
        val runtime = best match {
          case None => None
          case Some(value) => Some(value.runtime.get.value)
        }

        (solution.expression, runtime)
      }
      case Failure(s) => (solution.expression, None)
    }

  }

}
