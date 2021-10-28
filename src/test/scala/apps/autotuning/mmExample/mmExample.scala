package apps.autotuning.mmExample

import apps.separableConvolution2D
import apps.separableConvolution2D.mulT
import arithexpr.arithmetic.{RangeAdd, RangeMul}
import rise.autotune.{tuningParam, wrapOclRun}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.{Expr, _}
import rise.core.primitives.{add, map, mapSeq, transpose, zip, let => _, _}
import rise.core.types.DataType._
import rise.core.types.{AddressSpace, Nat}
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq
import shine.OpenCL.{GlobalSize, LocalSize}

class mmExample extends test_util.Tests {

  // helper functions
  private val id = fun(x => x)
  private val mulT: ToBeTyped[Expr] = fun(x => fst(x) * snd(x))
  private val dot: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduce(add)(lf32(0.0f))
  ))
  private val dotSeq = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduceSeq(add)(lf32(0.0f))
  ))

  // the first matrix input is transposed

  // high-level expression
  val mmHighLevel: ToBeTyped[Expr] = depFun((n: Nat, m: Nat, o: Nat) => fun(
    (o`.`n`.`f32) ->: (o`.`m`.`f32) ->: (n`.`m`.`f32)
  )((at, b) =>
    transpose(at) |> map(fun(aRow =>
      transpose(b) |> map(fun(bCol =>
        dot(aRow)(bCol)
      ))
    ))
  ))

  // no rewriting
  // lowered to generate sequential cpu c-code
  val mmSequential: ToBeTyped[Expr] = depFun((n: Nat, m: Nat, o: Nat) => fun(
    (o`.`n`.`f32) ->: (o`.`m`.`f32) ->: (n`.`m`.`f32)
  )((at, b) =>
    transpose(at) |> mapSeq(fun(aRow =>
      transpose(b) |> mapSeq(fun(bCol =>
        dotSeq(aRow)(bCol)
      ))
    ))
  ))

  // rewritten expression including tuning parameters
  // lowered to target gpu platform
  val mmTuning: ToBeTyped[Expr] =
    tuningParam("v3", RangeAdd(1, 1024, 1), (v3: Nat) =>
      tuningParam("v4", RangeAdd(1, 1024, 1), (v4: Nat) =>
        tuningParam("v5", RangeAdd(1, 1024, 1), (v5: Nat) =>
          tuningParam("v6", RangeAdd(1, 1024, 1), (v6: Nat) =>
            tuningParam("v7", RangeAdd(1, 1024, 1), (v7: Nat) =>
              tuningParam("v8", RangeAdd(1, 1024, 1), (v8: Nat) =>

                //    A(o,n) x B(o,m)
                //    v3 // divides v7
                //    v4 // divides v5
                //    v5 // tile-width A
                //    v6 // divides v8 x v5
                //    v7 // tile-width B
                //    v8 // tile-height A,B

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
                            // FIXME: there seems to be a bug in AdjustArraySizesForAllocations
                            oclReduceSeq(AddressSpace.Private)(fun((p13, p14) =>
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
                ))
              ))))))

  // wrap ocl run pattern for opencl-code-gen
  // add global and local sizes as tuning parameters
  val mm: Expr = {
  tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
    tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
      tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
        tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
          wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmTuning)
        ))))
  }
}
