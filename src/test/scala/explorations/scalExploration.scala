package explorations

import rise.core.DSL._
import rise.core.primitives._
import Type._
import elevate.core.strategies.traversal.bottomUp
import exploration.strategies.defaultStrategiesGPU
import rise.autotune.HostCode
import rise.core.types._
import rise.core.types.DataType._
import rise.elevate.rules.algorithmic.splitJoin
import rise.elevate.rules.traversal.default.RiseTraversable
import rise.elevate.tunable

import exploration.riseExploration

object scalExploration {


  // scalastyle:off
  val init: (Int) => String = (N) => {
    s"""
       |const int N = ${N};
       |
       |srand(time(NULL));
       |
       |Buffer inputV = createBuffer(ctx, N * sizeof(float), HOST_WRITE | DEVICE_READ);
       |Buffer outputV = createBuffer(ctx, N * sizeof(float), HOST_READ | DEVICE_WRITE);

       |float* inV = hostBufferSync(ctx, inputV, N * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N; i++) {
       |  inV[i] = (float)(rand());
       |}
       |
       |int scal = (float)(rand());
       |
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, outputV, N, inputV, scal);
       |""".stripMargin

  val finish =
    s"""
       |// TODO: could check output here
       |
       |destroyBuffer(ctx, inputV);
       |destroyBuffer(ctx, outputV);
       |""".stripMargin
  // scalastyle:on


  val scal = depFun((n: Nat) => fun(n `.` f32)(input => fun(f32)(alpha =>
    input |> map(fun(x => alpha * x)))
  ))

  val sjtp = bottomUp(tunable(splitJoin))(RiseTraversable)
  val scal2 = sjtp.apply(scal).get
  val scal3 = sjtp.apply(scal2).get
  val scal4 = sjtp.apply(scal3).get

  // try different lowering options
  // 1D
  val lowered1 = exploration.strategies.defaultStrategiesGPU.lowerGs0.apply(scal)
  val lowered2 = exploration.strategies.defaultStrategiesGPU.lowerWrgLcl.apply(scal2)

  // 2D
  val lowered3 = exploration.strategies.defaultStrategiesGPU.lowerGsGs.apply(scal2)
  val lowered4 = exploration.strategies.defaultStrategiesGPU.lowerWrgWrgLclLcl.apply(scal4)

  println("lowered1: " + lowered1)
  println("lowered2: " + lowered2)
  println("lowered3: " + lowered3)
  println("lowered4: " + lowered4)

  // different lowering strategies
  // strategies introducing tuning parameters
  // map -> mapSeq (all to mapSeq)
  // map, (map, ...) -> MapGlb, (mapSeq, mapSeq, ...) (first mapGlb, rest mapSeq)
  // map, map, (map, ...) -> MapLcl, MapWrg, mapSeq (first mapLcl, second mapWrg, rest mapSeq)

  def main(args: Array[String]): Unit = {
    // start exploration here
    riseExploration(
      scal,
      defaultStrategiesGPU.lowering,
      defaultStrategiesGPU.strategies,
      "exploration/configuration/scal.json",
      Some(HostCode(init(1024), compute, finish))
    )
  }
}
