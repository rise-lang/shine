package apps.stencil

import rise.core.DSL.HighLevelConstructs.{padClamp3D, scatterShift, slide3D}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._

object j3d13pt {
  private val jacobi = foreignFun("jacobi",
    Seq("EE", "E", "W", "WW", "SS", "S", "N", "NN", "BB", "B", "F", "FF", "C"),
    """return 0.083f * EE + 0.083f * E + 0.083f * W + 0.083f * WW +
      |       0.083f * SS + 0.083f * S + 0.083f * N + 0.083f * NN +
      |       0.083f * BB + 0.083f * B + 0.083f * F + 0.083f * FF -
      |       0.996f * C;""".stripMargin,
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
    f32 ->: f32 ->: f32 ->: f32)

  val j3d13ptHighLevel: ToBeTyped[Expr] = depFun((o: Nat, n: Nat, m: Nat) => fun(
    (o `.` n `.` m `.` f32) ->: (o `.` n `.` m `.` f32)
  )(input => input |>
    slide3D(5, 1) >> map(map(map(fun { nbh =>
      def at(i: Int, j: Int, k: Int): ToBeTyped[Expr] =
        nbh `@` lidx(i, 5) `@` lidx(j, 5) `@` lidx(k, 5)

      val ee = at(2, 2, 4)
      val e  = at(2, 2, 3)
      val w  = at(2, 2, 1)
      val ww = at(2, 2, 0)
      val ss = at(2, 4, 2)
      val s  = at(2, 3, 2)
      val n  = at(2, 1, 2)
      val nn = at(2, 0, 2)
      val bb = at(4, 2, 2)
      val b  = at(3, 2, 2)
      val f  = at(1, 2, 2)
      val ff = at(0, 2, 2)
      val c  = at(2, 2, 2)

      jacobi(ee)(e)(w)(ww)(ss)(s)(n)(nn)(bb)(b)(f)(ff)(c)
    }))) >> padClamp3D(2) >>
    scatterShift(2) >> map(scatterShift(2) >> map(scatterShift(2)))
    // TODO: map(map(padEmpty(4) >> padEmpty(4)) >> padEmpty(4) instead ?
  ))
}