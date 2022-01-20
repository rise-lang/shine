package apps.stencil

import rise.core.Expr
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.DSL.HighLevelConstructs.{padClamp2D, scatterShift, slide2D}
import rise.core.primitives._
import rise.core.types._
import rise.core.types.DataType._
import rise.openCL.DSL._

object gaussian {
  private val jacobi = foreignFun("jacobi", Seq(
    "NNWW", "NNW", "NN", "NNE", "NNEE",
    "NWW", "NW", "N", "NE", "NEE",
    "WW", "W", "C", "E", "EE",
    "SWW", "SW", "S", "SE", "SEE",
    "SSWW", "SSW", "SS", "SSE", "SSEE"),
    """return (2*NNWW + 4*NNW + 5*NN + 4*NNE + 2*NNEE +
      | 4*NWW + 9*NW + 12*N + 9*NE + 4*NEE +
      | 5*WW + 12*W + 15*C + 12*E + 5*EE +
      | 4*SWW + 9*SW + 12*S + 9*SE + 4*SEE +
      | 2*SSWW + 4*SSW + 5*SS + 4*SSE + 2*SSEE) / 159;""".stripMargin,
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
      f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
      f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
      f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
      f32 ->: f32 ->: f32 ->: f32 ->: f32 ->:
      f32)

  val gaussianHighLevel: ToBeTyped[Expr] = depFun((n: Nat, m: Nat) => fun(
    (n `.` m `.` f32) ->: (n `.` m `.` f32)
  )(input => input |>
    slide2D(5, 1) >> map(map(fun { nbh =>
      def at(i: Int, j: Int): ToBeTyped[Expr] =
        nbh `@` lidx(i, 5) `@` lidx(j, 5)
      val nnww = at(0, 0)
      val nnw  = at(0, 1)
      val nn   = at(0, 2)
      val nne  = at(0, 3)
      val nnee = at(0, 4)
      val nww  = at(1, 0)
      val nw   = at(1, 1)
      val n    = at(1, 2)
      val ne   = at(1, 3)
      val nee  = at(1, 4)
      val ww   = at(2, 0)
      val w    = at(2, 1)
      val c    = at(2, 2)
      val e    = at(2, 3)
      val ee   = at(2, 4)
      val sww  = at(3, 0)
      val sw   = at(3, 1)
      val s    = at(3, 2)
      val se   = at(3, 3)
      val see  = at(3, 4)
      val ssww = at(4, 0)
      val ssw  = at(4, 1)
      val ss   = at(4, 2)
      val sse  = at(4, 3)
      val ssee = at(4, 4)

      jacobi(nnww)(nnw)(nn)(nne)(nnee)(
        nww)(nw)(n)(ne)(nee)(
        ww)(w)(c)(e)(ee)(
        sww)(sw)(s)(se)(see)(
        ssww)(ssw)(ss)(sse)(ssee)
    })) >> padClamp2D(2, 2) >> scatterShift(2) >> map(scatterShift(2))
    // TODO: map(padEmpty(4)) >> padEmpty(4) instead ?
  ))

  val v__0 = 1
  val v__1 = 2
  val gaussianKeplerBest: ToBeTyped[Expr] = depFun((n: Nat, m: Nat) => fun(
    (n `.` m `.` f32) ->: (n `.` m `.` f32)
  )(input => input |>
    slide2D(4+v__0, v__0, 4+v__1, v__1) >>
    slide2D(5, 1) >> mapGlobal(1)(mapGlobal(0)(mapSeq(mapSeq(fun { nbh =>
      def at(i: Int, j: Int): ToBeTyped[Expr] =
        nbh `@` lidx(i, 5) `@` lidx(j, 5)
      val nnww = at(0, 0)
      val nnw  = at(0, 1)
      val nn   = at(0, 2)
      val nne  = at(0, 3)
      val nnee = at(0, 4)
      val nww  = at(1, 0)
      val nw   = at(1, 1)
      val n    = at(1, 2)
      val ne   = at(1, 3)
      val nee  = at(1, 4)
      val ww   = at(2, 0)
      val w    = at(2, 1)
      val c    = at(2, 2)
      val e    = at(2, 3)
      val ee   = at(2, 4)
      val sww  = at(3, 0)
      val sw   = at(3, 1)
      val s    = at(3, 2)
      val se   = at(3, 3)
      val see  = at(3, 4)
      val ssww = at(4, 0)
      val ssw  = at(4, 1)
      val ss   = at(4, 2)
      val sse  = at(4, 3)
      val ssee = at(4, 4)

      jacobi(nnww)(nnw)(nn)(nne)(nnee)(
        nww)(nw)(n)(ne)(nee)(
        ww)(w)(c)(e)(ee)(
        sww)(sw)(s)(se)(see)(
        ssww)(ssw)(ss)(sse)(ssee)
    })))) >> padClamp2D(2, 2) >> scatterShift(2) >> map(scatterShift(2))
    // TODO: map(padEmpty(4)) >> padEmpty(4) instead ?
  ))
}