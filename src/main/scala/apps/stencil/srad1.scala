package apps.stencil

import rise.core.Expr
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.DSL.HighLevelConstructs.{padClamp2D, slide2D}
import rise.core.primitives.{let => _, _}
import rise.core.types.DataType._
import rise.core.types._
import rise.openCL.DSL._

object srad1 {
  private val calculateScoeff = foreignFun("calculateScoeff",
    Seq("dN", "dS", "dE", "dW", "jC", "q0sqr"),
    """{
      | float g2 = (dN*dN + dS*dS + dW*dW + dE*dE) / (jC * jC);
      | float l = (dN + dS + dW + dE ) / jC;
      | float num = (0.5*g2) - ((1.0/16.0)*(l*l));
      | float den = 1 + (0.25*l);
      | float qsqr = num/(den*den);
      | den = (qsqr-q0sqr) / (q0sqr * (1+q0sqr));
      | float coeff = 1.0 / (1.0+den);
      | if (coeff > 1) {
      |   return 1.0f;
      | } else if (coeff < 0) {
      |   return 0.0f;
      | } else {
      |   return coeff;
      | }
      |}""".stripMargin,
    f32 ->: f32 ->: f32 ->: f32 ->: f32 ->: f32 ->: f32)

  val srad1HighLevel: ToBeTyped[Expr] = depFun((n: Nat, m: Nat) => fun(
    (n `.` m `.` f32) ->: (n `.` m `.` f32)
  )(image => image |>
    padClamp2D(1, 1) >> slide2D(3, 1) >> map(map(fun { nbh =>
      val q0sqr = lf32(0.053787220269f) // this value is dependent on data set size !!

      def m(i: Int, j: Int): ToBeTyped[Expr] = nbh `@` lidx(i, 3) `@` lidx(j, 3)

      val Jc = m(1, 1)
      val JW = m(1, 0)
      val JN = m(0, 1)
      val JS = m(2, 1)
      val JE = m(1, 2)

      val DW = JW - Jc
      val DN = JN - Jc
      val DS = JS - Jc
      val DE = JE - Jc
      calculateScoeff(DN)(DS)(DE)(DW)(Jc)(q0sqr)
    }))
  ))

  val srad1Nvidia: ToBeTyped[Expr] = depFun((n: Nat, m: Nat) => fun(
    (n `.` m `.` f32) ->: (n `.` m `.` f32)
  )(image => image |>
    padClamp2D(1, 1) >> slide2D(3, 1) >> mapGlobal(mapGlobal(fun { nbh =>
      val q0sqr = lf32(0.053787220269f) // this value is dependent on data set size !!

      def m(i: Int, j: Int): ToBeTyped[Expr] = nbh `@` lidx(i, 3) `@` lidx(j, 3)

      let(toPrivate(m(1, 1))) be { Jc =>
        let(toPrivate(m(1, 0))) be { JW =>
          let(toPrivate(m(0, 1))) be { JN =>
            let(toPrivate(m(2, 1))) be { JS =>
              let(toPrivate(m(1, 2))) be { JE =>

                val DW = JW - Jc
                val DN = JN - Jc
                val DS = JS - Jc
                val DE = JE - Jc
                calculateScoeff(DN)(DS)(DE)(DW)(Jc)(q0sqr)
              }
            }
          }
        }
      }
    }))
  ))
}
