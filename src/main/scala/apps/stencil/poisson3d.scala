package apps.stencil

import rise.core.DSL.HighLevelConstructs.{padClamp3D, scatterShift, slide3D}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._

object poisson3d {
  private val poisson = foreignFun("poisson", Seq(
    "C", "N", "S", "E", "W", "F", "B",
    "FN", "BN", "FS", "BS", "FW", "BW",
    "NW", "SW", "FE", "BE", "NE", "SE"),
    """return 2.666f * C - 0.166f * (F + B + N + S + E + W) -
      |       0.0833f * (FN + BN + FS + BS + FW + BW +
      |                  NW + SW + FE + BE + NE + SE);""".stripMargin,
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
    f32 ->: f32 ->: f32 ->: f32 ->: f32)

  val poisson3dHighLevel: ToBeTyped[Expr] = depFun((o: Nat, n: Nat, m: Nat) => fun(
    (o `.` n `.` m `.` f32) ->: (o `.` n `.` m `.` f32)
  )(input => input |>
    slide3D(3, 1) >> map(map(map(fun { nbh =>
      def at(i: Int, j: Int, k: Int): ToBeTyped[Expr] =
        nbh `@` lidx(i, 3) `@` lidx(j, 3) `@` lidx(k, 3)

      val c  = at(1, 1, 1)
      val n  = at(1, 0, 1)
      val s  = at(1, 2, 1)
      val e  = at(1, 1, 2)
      val w  = at(1, 1, 0)
      val f  = at(0, 1, 1)
      val b  = at(2, 1, 1)
      val fn = at(0, 0, 1)
      val bn = at(2, 0, 1)
      val fs = at(0, 2, 1)
      val bs = at(2, 2, 1)
      val fw = at(0, 1, 0)
      val bw = at(2, 1, 0)
      val nw = at(1, 0, 0)
      val sw = at(1, 2, 0)
      val fe = at(0, 1, 2)
      val be = at(2, 1, 2)
      val ne = at(1, 0, 2)
      val se = at(1, 2, 2)

      poisson(c)(n)(s)(e)(w)(f)(b)(fn)(bn)(fs)(bs)(fw)(bw)(nw)(sw)(fe)(be)(ne)(se)
    }))) >> padClamp3D(1) >>
    scatterShift(1) >> map(scatterShift(1) >> map(scatterShift(1)))
    // TODO: map(map(padEmpty(4) >> padEmpty(4)) >> padEmpty(4) instead ?
  ))
}