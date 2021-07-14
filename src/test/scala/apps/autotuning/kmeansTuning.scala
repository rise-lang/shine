package apps.autotuning

import rise.autotune
import rise.autotune.{HostCode, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core._
import rise.core.primitives._
import rise.core.types._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq
import shine.OpenCL.{GlobalSize, LocalSize}

class kmeansTuning extends test_util.Tests {

  private val update = fun(f32 ->: (f32 x f32) ->: f32)((dist, pair) =>
    dist + (pair._1 - pair._2) * (pair._1 - pair._2)
  )

  private val testF = foreignFun("test",
    Seq("dist", "tuple"),
    """{
      | float min_dist = tuple._fst;
      | int i = tuple._snd._fst;
      | int index = tuple._snd._snd;
      | if (dist < min_dist) {
      |   return (struct Record_float__int_int_){ dist, { i + 1 , i } };
      | } else {
      |   return (struct Record_float__int_int_){ min_dist, { i + 1, index } };
      | }
      }""".stripMargin,
    f32 ->: (f32 x (int x int)) ->: (f32 x (int x int)))

  private val select = fun(tuple => tuple._2._2)

  val kmeansOcl: Expr = depFun((p: Nat, c: Nat, f: Nat) => fun(
    (f`.`p`.`f32) ->: (c`.`f`.`f32) ->: (p`.`int)
  )((features, clusters) =>
    features |> transpose |> mapGlobal(fun(feature =>
      clusters |> oclReduceSeq(AddressSpace.Private)(
        fun(tuple => fun(cluster => {
          val dist = zip(feature)(cluster) |>
            oclReduceSeq(AddressSpace.Private)(update)(lf32(0.0f))
          testF(dist)(tuple)
        }))
      )(
        makePair(cast(lf64(3.40282347e+38)) :: f32)(makePair(l(0))(l(0)))
      ) |> select
    ))
  ))

  val kmeansTuning:Expr = {
    tuningParam("p", (p: Nat) =>
      tuningParam("c", (c: Nat) =>
        tuningParam("f", (f: Nat) =>
          fun(
            (f`.`p`.`f32) ->: (c`.`f`.`f32) ->: (p`.`int)
          )((features, clusters) =>
            features |> transpose |> mapGlobal(fun(feature =>
              clusters |> oclReduceSeq(AddressSpace.Private)(
                fun(tuple => fun(cluster => {
                  val dist = zip(feature)(cluster) |>
                    oclReduceSeq(AddressSpace.Private)(update)(lf32(0.0f))
                  testF(dist)(tuple)
                }))
              )(
                makePair(cast(lf64(3.40282347e+38)) :: f32)(makePair(l(0))(l(0)))
              ) |> select
            ))
          ))))
  }
  val kmeans:Expr =
    tuningParam("ls0", (ls0: Nat) =>
      tuningParam("gs0", (gs0: Nat) =>
        wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(kmeansTuning)))


  val init: (Int, Int, Int) => String = (p, c, f) => {
    s"""
       |  const int P = ${p};
       |  const int C = ${c};
       |  const int F = ${f};
       |  srand(time(NULL));
       |  Buffer features = createBuffer(ctx, F * P * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer clusters = createBuffer(ctx, C * F * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer output = createBuffer(ctx, P * sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |  float* in_features = hostBufferSync(ctx, features, F * P * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < F * P ; i++) {
       |    in_features[i] = (float)(rand() % 100);
       |  }
       |
       |  float* in_clusters = hostBufferSync(ctx, clusters, C * F * sizeof(float), HOST_WRITE);
       |    for (int i = 0; i < F * P ; i++) {
       |      in_clusters[i] = (float)(rand() % 100);
       |    }
       |
       |  deviceBufferSync(ctx, features, F * P * sizeof(float), DEVICE_READ);
       |  deviceBufferSync(ctx, clusters, C * F * sizeof(float), DEVICE_READ);
       |""".stripMargin
  }
  val compute =
    s"""
       |    fun_init_run(ctx, output, features, clusters);
       |""".stripMargin

  val finish =
    s"""
       |  // could add error checking
       |  destroyBuffer(ctx, features);
       |  destroyBuffer(ctx, clusters);
       |  destroyBuffer(ctx, output);
       |""".stripMargin


  test("execute kmeans"){

    println("kmeans: \n" + kmeans)

    val params = Map(
      TuningParameter("p") -> (1024: Nat),
      TuningParameter("c") -> (5: Nat),
      TuningParameter("f") -> (34: Nat),
      TuningParameter("ls0") -> (1: Nat),
      TuningParameter("gs0") -> (8: Nat)
    )

    val kmeans_replaced = rise.core.substitute.natsInExpr(params, kmeans)
    println("kmeans_replaced: \n" + kmeans_replaced)

    val result = autotune.execution.execute(
      expression = kmeans_replaced,
      hostCode = HostCode(init(1024, 5, 34), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100
    )

    println("result: " + result)
  }


  test("search nn"){

    val tuner = Tuner(
      hostCode = HostCode(init(1024, 5, 34), compute, finish),
      samples = 10,
      name = "kmeans",
      output = "autotuning/kmeans",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile =Some("/home/jo/development/rise-lang/shine/autotuning/config/kmeans_1024.json"),
      true
    )

    val tuningResult = autotune.search(tuner)(kmeans)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }

}
