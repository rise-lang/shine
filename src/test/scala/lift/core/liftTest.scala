package lift.core

import apps.harrisCornerDetection.{gaussian, mulT, sobelX, sobelY, sq, stencil3x3, szr, zip2D}
import lift.core.DSL._
import lift.core.HighLevelConstructs._
import lift.core.primitives._
import lift.core.semantics.Data
import lift.core.types._

class liftTest extends test_util.Tests {
  ignore("some random tests here") {
    val e = infer(nFun(szr, h => nFun(szr, w => fun(ArrayType(h, ArrayType(w, float)))
            (i => {
              val ix = stencil3x3(sobelX)(i)
              val sxx = gaussian(ix)
              sxx
            }))))

    def zip3D: Expr = zipND(3)

    val testZip3DOnce: Expr = infer(nFun(o => nFun(n => nFun(m => fun(
      (o`.`n`.`m`.`float) ->:
        (o`.`n`.`m`.`TupleType(float, float))
    )(mat => zip3D(mat)(mat))))))

    val testZip3DTwice: Expr = infer(nFun(o => nFun(n => nFun(m => fun(
      (o`.`n`.`m`.`float) ->:
        (o`.`n`.`m`.`TupleType(float, TupleType(float, float)))
    )(mat => zip3D(mat)(zip3D(mat)(mat)))))))

    val testZip3DComb: Expr = infer(nFun(o => nFun(n => nFun(m => fun(
      (o`.`n`.`m`.`float) ->:
        (o`.`n`.`m`.`TupleType(float, float)) ->:
        (o`.`n`.`m`.`TupleType(float, TupleType(float, float)))
    )((matx, maty) => zip3D(matx)(maty))))))
  }
}
