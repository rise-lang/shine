package exploration

import apps.separableConvolution2D.mulT
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


class mvExploration extends test_util.Tests {


  test("mvExploration"){
    val test = Array.empty[String]
    mvExploration.main(test)
  }


}

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

  def main(args: Array[String]): Unit = {
    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_tuner.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
//        riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_ii.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
//    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_exhaustive.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))
    //    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv/mv_debug.json", None)
  }

}
