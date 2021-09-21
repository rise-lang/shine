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

import scala.collection.mutable.ListBuffer

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
       |Buffer input = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer output = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
       |
       |float* m = hostBufferSync(ctx, input, N * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N; i++) {
       |  m[i] = (float)(rand())/(float)(RAND_MAX) * 10.0f;
       |}
       |float factor = 4;
       |
       |// synchronize before entering timed section
       |deviceBufferSync(ctx, input, N * sizeof(float), DEVICE_READ);
       |deviceBufferSync(ctx, output, N * sizeof(float), DEVICE_WRITE);
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, output, N, input, factor);
       |""".stripMargin

  val finish =
    s"""
       |// TODO: could check output here
       |
       |destroyBuffer(ctx, input);
       |destroyBuffer(ctx, output);
       |""".stripMargin


  def execute(solution: Solution[Rise]):(Rise,Option[Double]) = {

    // todo work with gold expression

    println("solution: " + solution)

    // create tuner
    val tuner = Tuner(
      hostCode = HostCode(init(1024), compute, finish),
      inputSizes = Seq(1024),
      samples = iterations,
      name = "scal",
      output = "autotuning/scal",
      timeouts = Timeouts(100000, 100000, 100000),
      executionIterations = 10,
      speedupFactor = threshold,
      None,
//      Some("/home/jo/development/rise-lang/shine/autotuning/scal/scal.json"),
      hierarchicalHM = true,
      runtimeStatistic = Median
    )

    // lower expression
    val lowered = lowering.apply(solution.expression)

    lowered match {
      case Success(p) => {

        // now wrap ocl
        val eTuning: Expr =
          tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
            tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
              tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
                tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
                  wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(lowered.get)
                ))))

        // run tuning
        val result = search(tuner)(eTuning)

        val best = getBest(result.samples)
        println("best: " + best)
        println("lowered: " + lowered)

        val runtime = best.get.runtime match{
          case Some(value) => Some(value.value)
          case None => None
        }

        (solution.expression, runtime)
      }
      case Failure(s) => (solution.expression, None)
    }

  }

}