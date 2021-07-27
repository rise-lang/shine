package shine.DPIA

import rise.core.DSL.Type.TypeConstructors
import rise.core.DSL.{TypeAnnotationHelper, fun}
import rise.core.primitives.{fst, makeProduct, mapSeq, snd}
import rise.core.types.{ArrayType, PairType, ProductType, f32}
import util.gen.c.function

class MultipleOutputs extends test_util.Tests {
  test("Program with tuples in output and tuple input, can be generated in C.") {
    val multipleOutputs =
      fun(ArrayType(8, PairType(f32, f32)) ->: ProductType(Seq(ArrayType(8, f32), ArrayType(8, f32))))(xs =>
        makeProduct(2)(
          xs |> mapSeq(fun(x => fst(x)))
        )(
          xs |> mapSeq(fun(x => snd(x)))
        )
      )

    println(function.asStringFromExpr(multipleOutputs))
  }
}
