package apps.stencil

import rise.core.DSL.HighLevelConstructs.{padClamp3D, scatterShift, slide3D}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._

object heat3d {
  private val heat = foreignFun("heat",
    Seq("C", "S", "N", "E", "W", "B", "F"),
    """return 0.125f * (B - 2.0f * C + F) +
      |       0.125f * (S - 2.0f * C + N) +
      |       0.125f * (E - 2.0f * C + W) + C;""".stripMargin,
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->: f32 ->: f32 ->: f32)

  val heat3dHighLevel: ToBeTyped[Expr] = depFun((o: Nat, n: Nat, m: Nat) => fun(
    (o `.` n `.` m `.` f32) ->: (o `.` n `.` m `.` f32)
  )(input => input |>
    slide3D(3, 1) >> map(map(map(fun { nbh =>
      def at(i: Int, j: Int, k: Int): ToBeTyped[Expr] =
        nbh `@` lidx(i, 3) `@` lidx(j, 3) `@` lidx(k, 3)
      // vonNeumann 7pt
      val N = at(1, 0, 1)
      val S = at(1, 2, 1)
      val W = at(1, 1, 0)
      val E = at(1, 1, 2)
      val C = at(1, 1, 1)
      val F = at(0, 1, 1)
      val B = at(2, 1, 1)

      heat(C)(N)(S)(E)(W)(B)(F)
    }))) >> padClamp3D(1) >>
    scatterShift(1) >> map(scatterShift(1) >> map(scatterShift(1)))
    // TODO: map(map(padEmpty(2) >> padEmpty(2)) >> padEmpty(2) instead ?
  ))
}