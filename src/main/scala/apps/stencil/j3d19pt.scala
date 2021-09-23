package apps.stencil

import rise.core.DSL.HighLevelConstructs.{padClamp3D, scatterShift, slide3D}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._

object j3d19pt {
  // FIXME: looks like 17pt?
  private val jacobi = foreignFun("jacobi", Seq(
    "FNW", "FNE", "FSW", "FSE",
    "NW", "N", "NE", "W", "C", "E", "SW", "S", "SE",
    "BNW", "BNE", "BSW", "BSE"),
    """return (0.5 * (FNW + FNE + FSW + FSE) +
      |        0.51 * NW + 0.71 * N + 0.91 * NE +
      |        1.21 * W + 1.51 * C + 1.21 * E +
      |        0.91 * SW + 0.71 * S + 0.51 * SE +
      |        0.52 * (BNW + BNE + BSW + BSE)) / 159;""".stripMargin,
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
    f32 ->: f32 ->: f32)

  val j3d19ptHighLevel: ToBeTyped[Expr] = depFun((o: Nat, n: Nat, m: Nat) => fun(
    (o `.` n `.` m `.` f32) ->: (o `.` n `.` m `.` f32)
  )(input => input |>
    slide3D(3, 1) >> map(map(map(fun { nbh =>
      def at(i: Int, j: Int, k: Int): ToBeTyped[Expr] =
        nbh `@` lidx(i, 3) `@` lidx(j, 3) `@` lidx(k, 3)

      val fnw = at(0, 0, 0)
      val fne = at(0, 0, 2)
      val fsw = at(0, 2, 0)
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
      val bne = at(2, 0, 2)
      val bsw = at(2, 2, 0)
      val bse = at(2, 2, 2)

      jacobi(fnw)(fne)(fsw)(fse)(nw)(n)(ne)(w)(c)(e)(sw)(s)(se)(bnw)(bne)(bsw)(bse)
    }))) >> padClamp3D(1) >>
    scatterShift(1) >> map(scatterShift(1) >> map(scatterShift(1)))
    // TODO: map(map(padEmpty(4) >> padEmpty(4)) >> padEmpty(4) instead ?
  ))
}