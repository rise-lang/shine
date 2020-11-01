package rise.core

import rise.core.util.show.DrawTree._
import rise.core.dsl.HighLevelConstructs._
import rise.core.util.show.ShowRise._
import rise.core.dsl.Type._
import rise.core.dsl._
import rise.core.exprs.{App, DepApp, DepLambda, Expr, Identifier, Lambda, Literal, Primitive, TypeAnnotation}
import rise.core.exprs.primitives._
import rise.core.types._
import rise.opencl.dsl._
import rise.opencl.primitives.oclReduceSeqUnroll
import shine.DPIA.Nat

class showRise extends test_util.Tests {
  private val id = fun(x => x)

  private val dotElemWeights = fun((weights, elem) =>
    oclReduceSeqUnroll(AddressSpace.Private)(fun((acc, pair) => {
      val pixel = pair._1
      val weight = pair._2
      acc + (pixel * weight)
    }))(l(0.0f))(zip(join(elem))(weights))
  )

  private val blurXTiled2D: Expr = depFun((n: Nat) =>
    fun(
      (n `.` n `.` f32) ->: (17 `.` f32) ->: (n `.` n `.` f32)
    )((matrix, weights) =>
      unslide2D o mapWorkGroup(1)(
        mapWorkGroup(0)(
          fun(tile =>
            mapLocal(1)(mapLocal(0)(dotElemWeights(weights)))
              o slide2D(1, 1, 17, 1)
              $ toLocal(mapLocal(1)(mapLocal(0)(id))(tile))
          )
        )
      ) o slide2D(4, 4, 144, 128)
        o padClamp2D(0, 0, 8, 8) $ matrix
    )
  )

  test("show blurXTiled2D as an example") {
    val probe: Expr => Boolean = {
      case padClamp() => true
      case _          => false
    }
    val example = blurXTiled2D
    val show = trackWith(probe, example, 10, defaultUnicodeConfig)
    println(show)
  }

  test("compare the result with simple implementations") {
    object ShowRiseSimp {
      def showRiseSimp(e: Expr, cfg: UnicodeConfig): String =
        drawASTSimp(e).show(cfg)
      def drawASTSimp(e: Expr): UnicodeDraw = e match {
        case i: Identifier => line(i.name)
        case Lambda(x, e)  => block(s"λ${x.name}", drawASTSimp(e))
        case App(f, e)     => drawASTSimp(f) :+> drawASTSimp(e)
        case dl @ DepLambda(x, e) =>
          block(s"Λ${x.name}:${dl.kindName}", drawASTSimp(e))
        case DepApp(f, x)     => line(x.toString) <+: drawASTSimp(f)
        case Literal(d)       => line(d.toString)
        case TypeAnnotation(e, _) => drawASTSimp(e)
        case p: Primitive     => line(p.name)
      }
    }

    object LessBrackets {
      def lessBrackets(e: Expr): String = lessBrackets(e, wrapped = false)
      def lessBrackets(e: Expr, wrapped: Boolean): String = e match {

        case i: Identifier => i.name

        case Lambda(x, e) =>
          val xs = lessBrackets(x)
          val es = lessBrackets(e)
          if (wrapped) s"<λ$xs. $es>" else s"λ$xs. $es"

        case App(f, e) =>
          val fs = f match {
            case _: Lambda => lessBrackets(f, wrapped = true)
            case _         => lessBrackets(f)
          }
          val es = lessBrackets(e, wrapped = true)
          if (wrapped) s"($fs $es)" else s"$fs $es"

        case dl @ DepLambda(x, e) =>
          val xs = s"${x.name}:${dl.kindName}"
          val es = lessBrackets(e)
          if (wrapped) s"[Λ$xs. $es]" else s"Λ$xs. $es"

        case DepApp(f, x) =>
          val fs = f match {
            case _: DepLambda[_] => lessBrackets(f, wrapped = true)
            case _               => lessBrackets(f)
          }
          if (wrapped) s"($fs $x)" else s"$fs $x"

        case Literal(d) => d.toString

        case TypeAnnotation(e, _) => lessBrackets(e, wrapped)

        case p: Primitive => p.name
      }
    }

    val example = blurXTiled2D

    assert(
      showRiseCompact(example, 0, defaultUnicodeConfig)
        == ShowRiseSimp.showRiseSimp(example, defaultUnicodeConfig)
    )

    assert(
      showRiseCompact(example, 1024, defaultUnicodeConfig)
        == line(LessBrackets.lessBrackets(example)).show(defaultUnicodeConfig)
    )
  }

  test(
    "change the configuration " +
      "(rounded corners and extended horizontal connections)"
  ) {
    val probe: Expr => Boolean = {
      case _: Lambda => true
      case _         => false
    }
    val example = blurXTiled2D
    val show = trackWith(probe, example, 10, UnicodeConfig("│╭├╰├╩╦╬═ ──"))
    println(show)
  }
}
