package apps

import elevate.core._
import rise.GAP8.DSL.gap8Run
import rise.GAP8.primitives.{allocL1, allocL2, copyToL1, copyToL2, gap8hwConv3x3}
import rise.core.DSL.HighLevelConstructs.{slide2D, zipND}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{mapSeq, reduceSeq, let => _, _}
import rise.core.types.DataType.{ArrayType, i16, int, u32, u8}
import rise.core.types.{DataType, _}
import rise.elevate._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.strategies.predicate.isPrimitive
import rise.elevate.strategies.traversal._
import rise.openMP.primitives.mapPar
import shine.GAP8.Module.translateToString

// scalastyle:off
object tester {
  def main(args: Array[String]) = {

    val gapSqrt = foreignFun("gap_sqrt",
      Seq("a_nInput"),
      """
        | {
        |   uint32_t op  = a_nInput;
        |   uint32_t res = 0;
        |
        |   uint32_t one = 1u << 14;
        |   while (one > op){
        |     one >>= 2;
        |   }
        |   while (one != 0) {
        |     if (op >= res + one){
        |       op = op - (res + one);
        |       res = res +  2 * one;
        |     }
        |     res >>= 1;
        |     one >>= 2;
        |   }
        |   return res;
        | }
        |""".stripMargin,
      i16 ->: i16
    )

    val h: Nat = 240
    val w: Nat = 320

    val sobelWithoutPad: ToBeTyped[Rise] =
      fun(
        ArrayType(h, ArrayType(w, u8)) ->:
          ArrayType(3, ArrayType(3, int)) ->:
          ArrayType(3, ArrayType(3, int)) ->:
          ArrayType(h - 2, ArrayType(w - 2, u8))
      )((pic, hf, vf) =>
        gap8Run(8)(
        pic |>
          slide2D(sz = 3, st = 1) |>
          mapPar(mapPar(fun(submat =>
            zip(submat |> join)(hf |> join) |> map(fun(x => (cast(fst(x)) :: u32) * cast(snd(x)) :: u32)) |> rise.core.primitives.reduceSeqUnroll(add)(cast(l(0)) :: u32) |> letf(h =>
              zip(submat |> join)(vf |> join) |> map(fun(x => (cast(fst(x)) :: u32) * cast(snd(x)) :: u32)) |> rise.core.primitives.reduceSeqUnroll(add)(cast(l(0)) :: u32) |> letf(v =>
                cast(gapSqrt(h * h + v * v)) :: u8
              )
            )
          ))))
      )

    //println(translateToString(util.gen.gap8.hosted("SobelWOutPad").fromExpr(sobelWithoutPad)))

    //TODO: Check gapSqrt type, it's changed above just to fit this expression type
    val sobelWithoutPadHWCE: ToBeTyped[Rise] = {
      fun(
        ArrayType(h, ArrayType(w, i16)) ->:
          ArrayType(3, ArrayType(3, i16)) ->:
          ArrayType(3, ArrayType(3, i16)) ->:
          ArrayType(h - 2, ArrayType(w - 2, i16))
      )((pic, hf, vf) =>
        gap8Run(8)(
          //pic |>
            //pic is 320 x 240
            gap8hwConv3x3(0)(pic, hf) |> allocL2 |> letf(hconvres =>
              gap8hwConv3x3(0)(pic, vf) |> allocL2 |> letf(vconvres =>
                //hconvres and vconvres are 318x238
                zipND(2)(hconvres)(vconvres) |> mapPar(mapPar(fun(elems =>
                  gapSqrt(add(mul(fst(elems))(fst(elems)))(mul(snd(elems))(snd(elems))))
                )))
              )
            )
        )
      )
    }
    //println(translateToString(util.gen.gap8.hosted("SobelWOutPadHWCE").fromExpr(sobelWithoutPadHWCE)))


    val matAdd: ToBeTyped[Expr] = depFun((n: Nat, m: Nat) =>
      fun(ArrayType(n, ArrayType(m, u32)) ->: ArrayType(n, ArrayType(m, u32)) ->: ArrayType(n, ArrayType(m, u32)))((x, y) =>
        gap8Run(8)(
          zip(x)(y) |>
            map(fun(pair => zip(fst(pair))(snd(pair)))) |>
            mapPar(mapPar(fun(pair => fst(pair) + snd(pair))))
        )
      )
    )

    val conv: Strategy[Rise] =
      (gap8hwConvMerge `@` everywhere)


    val optimizationStrategy: Strategy[Rise] =
      (`map -> mapPar`       `@` outermost(isPrimitive(map)))  `;`
        (`map -> mapSeq`       `@` outermost(isPrimitive(map)))  `;`
        (`reduce -> reduceSeq` `@` everywhere)

/*
    val anotherOptimizationStrategy: Strategy[Rise] =
      (`map >> reduce -> reduce` `@` everywhere) `;`
        (`map -> mapSeq`           `@` everywhere) `;`
        (`reduce -> reduceSeq`     `@` everywhere)

    val myStrategy: Strategy[Rise] =
      (`map >> reduce -> reduce` `@` everywhere) `;`
        (`map -> mapPar` `@` outermost(isPrimitive(map))) `;`
        (`map -> mapSeq` `@` everywhere)

    val yetAnotherOptimizationStrategy: Strategy[Rise] =
      innermost(isAppliedMap)(
        `map(f) -> asVector >> map(f_vec) >> asScalar`(4) `;`
          (`map -> mapSeq` `@` innermost(isPrimitive(map))) `;`
          storeTempAsVectors
      ) `;`
        optimizationStrategy*/
  }
}
