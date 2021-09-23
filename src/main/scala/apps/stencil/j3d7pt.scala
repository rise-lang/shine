package apps.stencil

import rise.core.DSL.HighLevelConstructs.{padClamp3D, scatterShift, slide3D}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._

object j3d7pt {
  private val jacobi = foreignFun("jacobi",
    Seq("C", "N", "S", "E", "W", "F", "B"),
    """return 0.161f * E + 0.162f * W +
      |       0.163f * S + 0.164f * N +
      |       0.165f * B + 0.166f * F -
      |       1.67f * C;""".stripMargin,
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
    f32 ->: f32 ->: f32)

  val j3d7ptHighLevel: ToBeTyped[Expr] = depFun((o: Nat, n: Nat, m: Nat) => fun(
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

      jacobi(C)(N)(S)(E)(W)(F)(B)
    }))) >> padClamp3D(1) >>
    scatterShift(1) >> map(scatterShift(1) >> map(scatterShift(1)))
    // TODO: map(map(padEmpty(4) >> padEmpty(4)) >> padEmpty(4) instead ?
  ))
}