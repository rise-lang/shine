package apps.stencil

import rise.core.DSL.HighLevelConstructs.{padClamp3D, scatterShift, slide3D}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._

object j3d27pt {
  private val jacobi = foreignFun("jacobi", Seq(
    "FNW", "FN", "FNE", "FW", "F", "FE", "FSW", "FS", "FSE",
    "NW", "N", "NE", "W", "C", "E", "SW", "S", "SE",
    "BNW", "BN", "BNE", "BW", "B", "BE", "BSW", "BS", "BSE"),
    """return (0.5 * FNW + 0.7 * FN + 0.9 * FNE +
      |        1.2 * FW + 1.5 * F + 1.2 * FE +
      |        0.9 * FSW + 0.7 * FS + 0.5 * FSE +
      |        0.51 * NW + 0.71 * N + 0.91 * NE +
      |        1.21 * W + 1.51 * C + 1.21 * E +
      |        0.91 * SW + 0.71 * S + 0.51 * SE +
      |        0.52 * BNW + 0.72 * BN + 0.92 * BNE +
      |        1.22 * BW + 1.52 * B + 1.22 * BE +
      |        0.92 * BSW + 0.72 * BS + 0.52 * BSE) / 159;""".stripMargin,
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
    f32 ->: f32 ->: f32)

  val j3d27ptHighLevel: ToBeTyped[Expr] = depFun((o: Nat, n: Nat, m: Nat) => fun(
    (o `.` n `.` m `.` f32) ->: (o `.` n `.` m `.` f32)
  )(input => input |>
    slide3D(3, 1) >> map(map(map(fun { nbh =>
      def at(i: Int, j: Int, k: Int): ToBeTyped[Expr] =
        nbh `@` lidx(i, 3) `@` lidx(j, 3) `@` lidx(k, 3)

      val fnw = at(0, 0, 0)
      val fn  = at(0, 0, 1)
      val fne = at(0, 0, 2)
      val fw  = at(0, 1, 0)
      val f   = at(0, 1, 1)
      val fe  = at(0, 1, 2)
      val fsw = at(0, 2, 0)
      val fs  = at(0, 2, 1)
      val fse = at(0, 2, 2)

      val nw  = at(1, 0, 0)
      val n   = at(1, 0, 1)
      val ne  = at(1, 0, 2)
      val w   = at(1, 1, 0)
      val c   = at(1, 1, 1)
      val e   = at(1, 1, 2)
      val sw  = at(1, 2, 0)
      val s   = at(1, 2, 1)
      val se  = at(1, 2, 2)

      val bnw = at(2, 0, 0)
      val bn  = at(2, 0, 1)
      val bne = at(2, 0, 2)
      val bw  = at(2, 1, 0)
      val b   = at(2, 1, 1)
      val be  = at(2, 1, 2)
      val bsw = at(2, 2, 0)
      val bs  = at(2, 2, 1)
      val bse = at(2, 2, 2)

      jacobi(fnw)(fn)(fne)(fw)(f)(fe)(fsw)(fs)(fse)(
        nw)(n)(ne)(w)(c)(e)(sw)(s)(se)(
        bnw)(bn)(bne)(bw)(b)(be)(bsw)(bs)(bse)
    }))) >> padClamp3D(1) >>
    scatterShift(1) >> map(scatterShift(1) >> map(scatterShift(1)))
    // TODO: map(map(padEmpty(2) >> padEmpty(2)) >> padEmpty(2) instead ?
  ))
}