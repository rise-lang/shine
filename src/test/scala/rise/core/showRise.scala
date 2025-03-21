package rise.core

import rise.core.DrawTree._
import rise.core.DSL.HighLevelConstructs._
import rise.core.ShowRise._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import rise.core.types.DataType._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeqUnroll
import shine.DPIA.Nat

class showRise extends test_util.Tests {
  private val id = fun(x => x)

  private val dotElemWeights = fun((weights, elem) =>
    oclReduceSeqUnroll(AddressSpace.Private)(fun((acc, pair) => {
      val pixel = pair.`1`
      val weight = pair.`2`
      acc + (pixel * weight)
    }))(lf32(0.0f))(zip(join(elem))(weights))
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
    logger.debug(show)
  }

  test("compare the result with simple implementations") {
    object ShowRiseSimp {
      def showRiseSimp(e: Expr, cfg: UnicodeConfig): String =
        drawASTSimp(e).show(cfg)
      def drawASTSimp(e: Expr): UnicodeDraw = e match {
        case i: Identifier => line(i.name)
        case Lambda(x, e)  => block(s"λ${x.name}", drawASTSimp(e))
        case App(f, e)     => drawASTSimp(f) :+> drawASTSimp(e)
        case DepLambda(kind, x, e) => block(s"Λ${Kind.idName(kind, x)}:${kind.name}", drawASTSimp(e))
        case DepApp(_, f, x)     => line(x.toString) <+: drawASTSimp(f)
        case Literal(d)       => line(d.toString)
        case TypeAnnotation(e, _) => drawASTSimp(e)
        case TypeAssertion(e, _) => drawASTSimp(e)
        case Opaque(e, _) => drawASTSimp(e)
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

        case DepLambda(kind, x, e) =>
          val xs = s"${Kind.idName(kind, x)}:${kind.name}"
          val es = lessBrackets(e)
          if (wrapped) s"[Λ$xs. $es]" else s"Λ$xs. $es"

        case DepApp(_, f, x) =>
          val fs = f match {
            case _: DepLambda[_, _] => lessBrackets(f, wrapped = true)
            case _                     => lessBrackets(f)
          }
          if (wrapped) s"($fs $x)" else s"$fs $x"

        case Literal(d) => d.toString

        case TypeAnnotation(e, _) => lessBrackets(e, wrapped)
        case TypeAssertion(e, _) => lessBrackets(e, wrapped)
        case Opaque(e, _) => lessBrackets(e, wrapped)

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
    logger.debug(show)
  }
}
