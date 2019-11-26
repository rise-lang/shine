package lift.core

import lift.OpenCL.DSL._
import lift.core.DSL._
import lift.core.primitives._
import lift.core.DrawTree._
import lift.core.HighLevelConstructs._
import lift.core.types._
import lift.core.ShowLift._

class showLift extends test_util.Tests {
  private val id = fun(x => x)

  private val dotElemWeights = fun((weights, elem) =>
    oclReduceSeqUnroll(AddressSpace.Private)(fun((acc, pair) => {
      val pixel = pair._1
      val weight = pair._2
      acc + (pixel * weight)
    }))(l(0.0f))(zip(join(elem))(weights))
  )

  private val blurXTiled2D: Expr = nFun(n => fun(
    (n`.`n`.`float) ->: (17`.`float) ->: (n`.`n`.`float)
  )((matrix, weights) =>
    unslide2D o mapWorkGroup(1)(mapWorkGroup(0)(fun(tile =>
      mapLocal(1)(mapLocal(0)(dotElemWeights(weights)))
        o slide2D(1, 1, 17, 1)
        $ toLocal(mapLocal(1)(mapLocal(0)(id))(tile))
    ))) o slide2D(4, 4, 144, 128)
      o padClamp2D(0, 0, 8, 8) $ matrix
  ))

  test("show blurXTiled2D as an example") {
    val probe: Expr => Boolean = {
      case _: PadClamp => true
      case _ => false
    }
    val example = blurXTiled2D
    val show = trackWith(probe, example, 10, defaultUnicodeConfig)
    println(show)
    assert(show ==
      """─Λn6:nat─λe7─λe8─▶─λe28─▼─λe26─λe23─λe10. join (map <λe9. map join (transpose e9)> e10)
        |                 │      │      │    └─mapWorkGroup
        |                 │      │      │      ├─mapWorkGroup
        |                 │      │      │      │ └─λe19─λe22─mapLocal
        |                 │      │      │      │        │    ├─mapLocal
        |                 │      │      │      │        │    │ └─λe2─λe3─oclReduceSeqUnroll Private
        |                 │      │      │      │        │    │   │       ├─λe4. λe5. add e4 (mul (fst e5) (snd e5))
        |                 │      │      │      │        │    │   │       ├─FloatData(0.0)
        |                 │      │      │      │        │    │   │       └─zip (join e3) e2
        |                 │      │      │      │        │    │   └─e8
        |                 │      │      │      │        │    └─λe21─map transpose
        |                 │      │      │      │        │      │    └─<λe20. slide 1 1 (map (slide 17 1) e20)> e21
        |                 │      │      │      │        │      └─e22
        |                 │      │      │      │        └─toMem Local (mapLocal (mapLocal <λe1. e1>) e19)
        |                 │      │      │      └─e23
        |                 │      │      └─λe25─map transpose
        |                 │      │        │    └─<λe24. slide 4 4 (map (slide 144 128) e24)> e25
        |                 │      │        └─e26
        |                 │      └─<λe27. ⚑ padClamp 0 0 (map (⚑ padClamp 8 8) e27)> e28
        |                 └─e7""".stripMargin)
  }

  test("compare the result with simple implementations") {
    object ShowLiftSimp {
      def showLiftSimp(e: Expr, cfg: UnicodeConfig): String = drawASTSimp(e).show(cfg)
      def drawASTSimp(e: Expr): UnicodeDraw = e match {
        case i: Identifier => line(i.name)
        case Lambda(x, e) => block(s"λ${x.name}", drawASTSimp(e))
        case App(f, e) => drawASTSimp(f) :+> drawASTSimp(e)
        case dl @ DepLambda(x, e) => block(s"Λ${x.name}:${dl.kn.get}", drawASTSimp(e))
        case DepApp(f, x) => line(x.toString) <+: drawASTSimp(f)
        case l: Literal => line(l.toString)
        case p: Primitive => line(p.toString)
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
            case _ => lessBrackets(f)
          }
          val es = lessBrackets(e, wrapped = true)
          if (wrapped) s"($fs $es)" else s"$fs $es"

        case dl @ DepLambda(x, e) =>
          val xs = s"${x.name}:${dl.kn.get}"
          val es = lessBrackets(e)
          if (wrapped) s"[Λ$xs. $es]" else s"Λ$xs. $es"

        case DepApp(f, x) =>
          val fs = f match {
            case _: DepLambda[_] => lessBrackets(f, wrapped = true)
            case _ => lessBrackets(f)
          }
          if (wrapped) s"($fs $x)" else s"$fs $x"

        case l: Literal => l.toString

        case p: Primitive => p.toString
      }
    }

    val example = blurXTiled2D

    assert(showLiftCompact(example, 0, defaultUnicodeConfig)
      == ShowLiftSimp.showLiftSimp(example, defaultUnicodeConfig))

    assert(showLiftCompact(example, 1024, defaultUnicodeConfig)
      == line(LessBrackets.lessBrackets(example)).show(defaultUnicodeConfig))
  }

  test("change the configuration (rounded corners and extended horizontal connections)") {
    val probe: Expr => Boolean = {
      case _: Lambda => true
      case _ => false
    }
    val example = blurXTiled2D
    val show = trackWith(probe, example, 10, UnicodeConfig("│╭├╰├╩╦╬═ ──"))
    println(show)
    assert(show ==
    """──Λn6:nat──λe7──λe8──▶──λe28──◆──λe26──◆──λe23──◆──λe10. join (map <⚑ λe9. map join (transpose e9)> e10)
      |                     │        │        │        ╰──▶──▼──mapWorkGroup
      |                     │        │        │           │  ╰──▼──mapWorkGroup
      |                     │        │        │           │     ╰──λe19──◆──λe22──◆──▼──mapLocal
      |                     │        │        │           │              │        │  ╰──▼──mapLocal
      |                     │        │        │           │              │        │     ╰──▶──λe2──λe3──▶──▶──▼──oclReduceSeqUnroll Private
      |                     │        │        │           │              │        │        │            │  │  ╰──λe4. ⚑ λe5. add e4 (mul (fst e5) (snd e5))
      |                     │        │        │           │              │        │        │            │  ╰──FloatData(0.0)
      |                     │        │        │           │              │        │        │            ╰──zip (join e3) e2
      |                     │        │        │           │              │        │        ╰──e8
      |                     │        │        │           │              │        ╰──▶──λe21──▼──map transpose
      |                     │        │        │           │              │           │        ╰──<⚑ λe20. slide 1 1 (map (slide 17 1) e20)> e21
      |                     │        │        │           │              │           ╰──e22
      |                     │        │        │           │              ╰──toMem Local (mapLocal (mapLocal <⚑ λe1. e1>) e19)
      |                     │        │        │           ╰──e23
      |                     │        │        ╰──▶──λe25──▼──map transpose
      |                     │        │           │        ╰──<⚑ λe24. slide 4 4 (map (slide 144 128) e24)> e25
      |                     │        │           ╰──e26
      |                     │        ╰──<⚑ λe27. padClamp 0 0 (map (padClamp 8 8) e27)> e28
      |                     ╰──e7""".stripMargin
    )
  }
}
