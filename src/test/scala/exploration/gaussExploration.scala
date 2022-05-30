package exploration

import apps.separableConvolution2D.mulT
import exploration.mvExploration.writeMat
import exploration.strategies.defaultStrategiesGPU
import rise.autotune.HostCode
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._
import rise.openCL.DSL.mapGlobal
import rise.openCL.primitives.oclReduceSeq
import rise.core.DSL.HighLevelConstructs.{padClamp2D, slide2D, zipND}


class gaussExploration extends test_util.Tests {


  test("mvExploration"){
    val test = Array.empty[String]
    gaussExploration.main(test)
  }


}

object gaussExploration {

  val gaussWeights: Seq[Seq[Number]] = Seq(
    Seq(1, 4, 6, 4,1),
    Seq(4,16,24,16,4),
    Seq(6,24,36,24,6),
    Seq(4,16,24,16,4),
    Seq(1, 4, 6, 4,1)
  )
  val wX = gaussWeights.size
  val wY = gaussWeights(1).size

  val N = 128
  val M = 128

  val mulPair = fun(pair => fst(pair) * snd(pair))

  val gauss = {
    fun(N `.` M `.` f32)(in =>
      fun( wX `.` wY `.` f32)(weights =>
        in |> padClamp2D(2) // in: NxM -> (N+4) x (M+4)
          |> slide2D(5, 1) // -> MxN of 5x5 slides
          |> map(map(fun(sector => // sector:5x5
          zip(sector |> join)(weights |> join) |> map(mulPair) |> reduce(add)(lf32(0)) |> fun(x => x/lf32(256))
        )))
      )
    )
  }


  object gaussHostCode {
    // scalastyle:off
    val init = {
      val mFile = writeMat(Mat.generate(N, M)(Math.random.toFloat))
      val wFile = writeMat(Mat.generate(wX, wY)(Math.random.toFloat))
      s"""
         |const int mSize = ${N * M};
         |const int wSize = ${wX * wY};
         |
         |
         |Buffer inputM = createBuffer(ctx, mSize* sizeof(float), HOST_WRITE | DEVICE_READ);
         |Buffer inputW = createBuffer(ctx, wSize * sizeof(float), HOST_WRITE | DEVICE_READ);
         |Buffer outputM = createBuffer(ctx, mSize * sizeof(float), HOST_READ | DEVICE_WRITE);
         |
         |float* inM = hostBufferSync(ctx, inputM, mSize* sizeof(float), HOST_WRITE);
         |FILE* file = fopen("${mFile.getAbsolutePath}", "r");
         |for (int i = 0; i < mSize ; i++) {
         |  fscanf(file, "%f", &inM[i]);
         |}
         |fclose(file);
         |
         |float* inW = hostBufferSync(ctx, inputW, wSize * sizeof(float), HOST_WRITE);
         |file = fopen("${wFile.getAbsolutePath}", "r");
         |for (int i = 0; i < wSize; i++) {
         |  fscanf(file, "%f", &inW[i]);
         |}
         |fclose(file);
         |
         |
         |""".stripMargin
    }

    val compute =
      s"""
         |fun_run(ctx, &fun, outputM, inputM, inputW);
         |""".stripMargin

    val finish =
      s"""
         |// TODO: could check output here
         |
         |destroyBuffer(ctx, inputM);
         |destroyBuffer(ctx, inputW);
         |destroyBuffer(ctx, outputM);
         |""".stripMargin
    // scalastyle:on
  }

  def main(args: Array[String]): Unit = {
    //println(defaultStrategiesGPU.lowerGs0.apply(gauss))


    riseExploration(gauss, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_tuner.json", Some(HostCode(gaussHostCode.init, gaussHostCode.compute, gaussHostCode.finish)))
    //    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_tuner_debug.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
    //        riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_ii.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
    //    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_exhaustive.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
    //    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_exhaustive_debug.json", None)
    //    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_debug.json", None)
  }

}
