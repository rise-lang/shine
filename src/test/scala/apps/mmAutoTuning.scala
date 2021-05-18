package apps


import arithexpr.arithmetic.{RangeAdd, RangeMul}
import rise.autotune
import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq
import rise.autotune.{Tuner, collectConstraints, collectParameters, tuningParam, wrapOclRun}
import shine.OpenCL.{GlobalSize, LocalSize}


class mmAutoTuning extends test_util.Tests {

  val id = fun(x => x)

  val main: (Int, Int, Int) => String = (N,M,O) => {
    s"""
    const int N = ${N};
    const int M = ${M};
    const int O = ${O};
    int main(int argc, char** argv) {
      Context ctx = createDefaultContext();
      Buffer inputA = createBuffer(ctx, N * M * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
      Buffer inputB = createBuffer(ctx, M * O * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
      Buffer outputC = createBuffer(ctx, N * O *  sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);

      float* inA = hostBufferSync(ctx, inputA, N * M * sizeof(float), HOST_WRITE);
      for (int i = 0; i < N; i++) {
        inA[i] = 1;
      }

      float* inB = hostBufferSync(ctx, inputB, M * O * sizeof(float), HOST_WRITE);
      for (int i = 0; i < N; i++) {
        inB[i] = 1;
      }

      foo_init_run(ctx, outputC, inputA, inputB);

      float* out = hostBufferSync(ctx, output, N * O * sizeof(float), HOST_READ);

//    todo add error checking

      destroyBuffer(ctx, inputA);
      destroyBuffer(ctx, inputB);
      destroyBuffer(ctx, outputC);
      destroyContext(ctx);
      return EXIT_SUCCESS;
    }
    """
  }

  val N = 64
  val M = 128
  val O = 128


  val mmNVIDIA: ToBeTyped[Expr] = {

    // A(o,n) x B(o,m)
    // val v5 = 64 // tile-width A
    // val v7 = 128 // tile-width B
    // val v8 = 16 // tile-height A,B

    // val v3 = 4 // divides v7
    // val v4 = 8 // divides v5
    // val v6 = 128 // divides v8 x v5

    //    val v3 = 4 // divides v7
    //    val v4 = 8 // divides v5
    //    val v5 = 64 // tile-width A
    //    val v6 = 128 // divides v8 x v5
    //    val v7 = 128 // tile-width B
    //    val v8 = 16 // tile-height A,B

    tuningParam("v3", RangeMul(1, 128, 2), (v3: Nat) =>
      tuningParam("v4", RangeMul(1, 128, 2), (v4: Nat) =>
        tuningParam("v5", RangeMul(1, 128, 2), (v5: Nat) =>
          tuningParam("v6", RangeMul(1, 128, 2), (v6: Nat) =>
            tuningParam("v7", RangeMul(1, 128, 2), (v7: Nat) =>
              tuningParam("v8", RangeMul(1, 128, 2), (v8: Nat) =>
                depFun((n: Nat, m: Nat, o: Nat) => fun(
                  (o`.`n`.`f32) ->: (o`.`m`.`f32) ->: (n`.`m`.`f32)
                )((at, b) =>
                  at |>
                    map(split(v5)) |> split(v8) |> // O'.v8.N'.v5.f
                    map(transpose) |> transpose |> // N'.O'.v8.v5.f
                    mapWorkGroup(1)(fun(p2 =>
                      b |>
                        map(split(v7)) |> split(v8) |> // O'.v8.M'.v7.f
                        map(transpose) |> transpose |> // M'.O'.v8.v7.f
                        mapWorkGroup(0)(fun(p3 =>
                          zip(p2)(p3) |> // O'.(v8.v5.f x v8.v7.f)
                            oclReduceSeq(AddressSpace.Local)(fun((p13, p14) =>
                              // (v5/^v4).(v7/^v3).v4.v3.f x (v8.v5.f x v8.v7.f)
                              let (toLocal(makePair(
                                p14._1 |> join |> split(v6) |> // ((v8 x v5) /^ v6).v6.f
                                  mapLocal(1)(asScalar o mapLocal(0)(id) o asVectorAligned(4)) |>
                                  join |> split(v5)
                              )( // v8.v5.f
                                p14._2 |> // v8.v7.f
                                  mapLocal(1)(asScalar o mapLocal(0)(id) o asVectorAligned(4))
                              )))
                                be (p15 =>
                                zip(p13)(split(v4)(transpose(p15._1))) |> // (v5/^v4).((v7/^v3).v4.v3.f x v4.v8.f)
                                  mapLocal(1)(fun(p16 =>
                                    zip(p16._1)(split(v3)(transpose(p15._2))) |> // (v7/^v3).(v4.v3.f x v3.v8.f)
                                      mapLocal(0)(fun(p17 =>
                                        zip(transpose(p16._2))(transpose(p17._2)) |> // v8.(v4.f x v3.f)
                                          oclReduceSeq(AddressSpace.Private)(fun((p19, p20) =>
                                            // v4.v3.f x (v4.f x v3.f)
                                            let (toPrivate(makePair(mapSeq(id)(p20._1))(mapSeq(id)(p20._2))))
                                              be (p21 =>
                                              zip(p19)(p21._1) |> // v4.(v3.f x f)
                                                mapSeq(fun(p22 =>
                                                  zip(p22._1)(p21._2) |> // v3.(f x f)
                                                    mapSeq(fun(p23 =>
                                                      p23._1 + (p22._2 * p23._2)
                                                    ))
                                                ))
                                              )
                                          ))(p17._1 // v4.v3.f
                                            |> mapSeq(mapSeq(id)) // TODO: think about that
                                          ) |> mapSeq(mapSeq(id)) // TODO: think about that
                                      ))
                                  ))
                                )
                            ))(
                              generate(fun(_ =>
                                generate(fun( _ =>
                                  generate(fun(_ =>
                                    generate(fun(_ => lf32(0.0f))))))))) |>
                                mapLocal(1)(mapLocal(0)(mapSeq(mapSeq(id))))
                            ) |> // (v5/^v4).(v7/^v3).v4.v3.f
                            mapLocal(1)(mapLocal(0)(mapSeq(asScalar o mapSeq(id) o asVector(4)))) |>
                            map(transpose) |> join |> map(join) |> transpose // v7.v5.f
                        )) |> join |> transpose // v5.M.f
                    )) |> join // N.M.f
                ))))))))
  }

  test("get constraints"){
    val e:Expr = mmNVIDIA
    println("e: \n" + e)

    val params = collectParameters(e)
    println("params: \n" + params)
    val constraints = collectConstraints(e, params)
    println("constraints: \n" + constraints)
  }

  test("wrap OCL run"){
    val e:Expr = mmNVIDIA

    val e2 = tuningParam("ls0", (ls0: Nat) => tuningParam("ls1", (ls1: Nat) =>
      tuningParam("gs0", (gs0: Nat) => tuningParam("gs1", (gs1: Nat) =>
        wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(e)))))

    println("e: \n " + e)
    println("e2: \n" + e2)

    //
    val tuner = Tuner(main(64, 128, 128), 10, "MM", "autotuning", Some("/home/jo/development/rise-lang/shine/autotuning/configs/mm.json"), Some("/home/jo/development/tuning/hypermapper_dev/hypermapper/optimizer.py"))
    val result = autotune.search(tuner)(e2)

    val best = autotune.getBest(result.samples)

    println("best: " + best)
  }

  test("execute mm"){
    val goodParameters = Map(
      NatIdentifier("gs0", isExplicit = true) -> (128: Nat),
      NatIdentifier("gs1", isExplicit = true) -> (64: Nat),
      NatIdentifier("ls0", isExplicit = true) -> (32: Nat),
      NatIdentifier("ls1", isExplicit = true) -> (32: Nat),
      NatIdentifier("v3", isExplicit = true) -> (4: Nat),
      NatIdentifier("v4", isExplicit = true) -> (8: Nat),
      NatIdentifier("v5", isExplicit = true) -> (64: Nat),
      NatIdentifier("v6", isExplicit = true) -> (128: Nat),
      NatIdentifier("v7", isExplicit = true) -> (128: Nat),
      NatIdentifier("v8", isExplicit = true) -> (16: Nat),
    )
    val e:Expr = mmNVIDIA
    val e2 = rise.core.substitute.natsInExpr(goodParameters, e)

    println("e: " + e)
    println("e2: " + e2)

    val result = autotune.execute(e2, main(64, 128, 128))
    println("result: " + result)
  }

  test("distribute constraints"){
    val e = wrapOclRun(LocalSize(32, 8), GlobalSize(32, 8))(mmNVIDIA)
    val params = autotune.collectParameters(e)
    val constraints = autotune.collectConstraints(e, params)

    val distribution = autotune.distributeConstraints(params, constraints)

    println("output: \n")
    distribution.foreach(elem => {
      println("elem: " + elem._1)
      println("dependencies: " + elem._2._2)
      println("constraints: " + elem._2._1)
      println()
    })

  }

}
//