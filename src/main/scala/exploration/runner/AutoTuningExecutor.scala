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

case class AutoTuningExecutor(lowerings: Set[Strategy[Rise]],
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

    println("solution: " + solution)

    // create tuner
    val tuner = Tuner(
      hostCode = HostCode(init(1024), compute, finish),
      inputSizes = Seq(1024),
      samples = 0,
      name = "scal",
      output = "autotuning/scal",
      timeouts = Timeouts(100000, 100000, 100000),
      executionIterations = 10,
      speedupFactor = 100,
      None,
//      Some("/home/jo/development/rise-lang/shine/autotuning/scal/scal.json"),
      hierarchicalHM = true,
      runtimeStatistic = Median
    )

    // lower expression
//    val e:RewriteResult[Rise] = lowering.apply(solution.expression)

    // apply scal lowerings
//
//    // 1 map
//    val e0 = exploration.strategies.scalStrategies.lowerGs.apply(solution.expression)
//    // 2 map
//    val e1 = exploration.strategies.scalStrategies.lowerGsGs.apply(solution.expression)
//    val e2 = exploration.strategies.scalStrategies.lowerWrgLcl.apply(solution.expression)
//    // 4 map
//    val e3 = exploration.strategies.scalStrategies.lowerWrgWrgLclLcl.apply(solution.expression)
//
////    val loweringsAll = Set(e0, e1, e2, e3)
////    val lowerings = loweringsAll.filter(elem => {
////      elem match{
////        case Success(p) => true
////        case Failure(s) => false
////      }
////    })

    val loweredExpressions = lowerings.map(lowering => lowering.apply(solution.expression))
      .filter(e => e match{
        case Success(p) => true
        case Failure(s) => false
      })

    var bestRuntime:Option[Double] = None

    // remove this later?
    val summary = ListBuffer.empty[(rise.elevate.Rise, Option[Double])]

    // now tune all lowerings
    loweredExpressions.foreach(e => {
      println("lowered: " + e.get)
      val lowered:Expr = e.get


      // now wrap ocl
//      val eTuning: Expr =
//        tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
//          tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
//            wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(lowered)
//          ))

      // now wrap ocl
      val eTuning: Expr =
        tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
          tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
            tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
              tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
                wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(lowered)
              ))))

      // run tuning
      val result = search(tuner)(eTuning)

      val best = getBest(result.samples)
      println("best: " + best)
      println("lowered: " + e)

      // report runtime
      val runtime = best match {
        case None => None
        case Some(value) => Some(value.runtime.get.value)
      }

      bestRuntime = bestRuntime match {
        case None => runtime
        case Some(value) => {
          runtime match {
            case Some(value2) => {
              if (value > value2) {
                Some(value2)
              }else{
                Some(value)
              }
            }
            case None => bestRuntime
          }
        }
      }

      summary += ((e.get:Expr, runtime))
    })

    println("resume: ")
    summary.foreach(elem => {
      println("expr: " + elem._1)
      println("runtime: " + elem._2)
    })

    (solution.expression, bestRuntime)

//
//    // check, if lowering was successful
//    e match{
//      case Success(p) => {
//        println("lowered: " + e.get)
//        val lowered:Expr = e.get
//
//
//        // now wrap ocl
//        val eTuning: Expr =
//          tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
//              tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
//                  wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(lowered)
//                ))
////
////        // now wrap ocl
////        val eTuning: Expr =
////          tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
////            tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
////              tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
////                tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
////                  wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(lowered)
////                ))))
//
//        // run tuning
//        val result = search(tuner)(eTuning)
//
//        val best = getBest(result.samples)
//        println("best: " + best)
//
//        // report runtime
//        val runtime = best match {
//          case None => None
//          case Some(value) => Some(value.runtime.get.value)
//        }
//
//        (solution.expression, runtime)
//      }
//      case Failure(s) => (solution.expression, None)
//    }

  }

}
