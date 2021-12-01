package apps.stencil

import rise.core.DSL.HighLevelConstructs.{padClamp2D, scatterShift, slide2D}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._

object j2d9pt {
  private val jacobi = foreignFun("jacobi",
    Seq("NW", "N", "NE", "W", "C", "E", "SW", "S", "SE"),
    "return (7 * NW + 5 * N + 9 * NE + 12 * W + 15 * C + 12 * E + 9 * SW + 5 * S + 7 * SE) / 118;",
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->: f32->: f32 ->: f32 ->: f32 ->: f32)

  val j2d9ptHighLevel: ToBeTyped[Expr] = depFun((n: Nat, m: Nat) => fun(
    (n `.` m `.` f32) ->: (n `.` m `.` f32)
  )(input => input |>
    slide2D(3, 1) >> map(map(fun { nbh =>
      def at(i: Int, j: Int): ToBeTyped[Expr] =
        nbh `@` lidx(i, 3) `@` lidx(j, 3)
      // moore 9pt
      val NW = at(0, 0)
      val N  = at(0, 1)
      val NE = at(0, 2)
      val W  = at(1, 0)
      val C  = at(1, 1)
      val E  = at(1, 2)
      val SW = at(2, 0)
      val S  = at(2, 1)
      val SE = at(2, 2)

      jacobi(NW)(N)(NE)(W)(C)(E)(SW)(S)(SE)
    })) >> padClamp2D(1, 1) >> scatterShift(1) >> map(scatterShift(1))
    // TODO: map(padEmpty(2)) >> padEmpty(2) instead ?
  ))
}