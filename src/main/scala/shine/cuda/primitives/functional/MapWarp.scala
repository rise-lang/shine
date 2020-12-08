package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.fedAcc
import shine.DPIA.DSL.{λ, _}
import shine.DPIA.FunctionalPrimitives.AbstractMapLoop
import shine.DPIA.ImperativePrimitives.MapAcc
import shine.DPIA.Phrases.{Identifier, Lambda, Phrase}
import shine.DPIA.Types._
import shine.DPIA._
import shine.cuda.primitives.intermediate.MapWarpI

final case class MapWarp(dim: Char)(
  override val n: Nat,
  override val dt1: DataType,
  override val dt2: DataType,
  override val f: Phrase[ExpType ->: ExpType],
  override val array: Phrase[ExpType]
) extends AbstractMapLoop(n, dt1, dt2, f, array)
{
  override def makeMap = MapWarp(dim)
  override def makeMapI(
                         n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         f: Phrase[->:[ExpType, ->:[AccType, CommType]]],
                         array: Phrase[ExpType],
                         out: Phrase[AccType])(
                         implicit context: TranslationContext
                       ): Phrase[CommType] = {
    MapWarpI(dim)(n, dt1, dt2, f, array, out)
  }

  override def fedeTranslation(env: scala.Predef.Map[Identifier[ExpType], Identifier[AccType]])
                              (C: Phrase[AccType ->: AccType]) : Phrase[AccType] = {
    val x = Identifier(freshName("fede_x"), ExpType(dt1, read))

    val otype = AccType(dt2)
    val o = Identifier(freshName("fede_o"), otype)

    fedAcc(env)(array)(λ(env.toList.head._2.t)(y =>
      MapAcc(n, dt2, dt1,
        Lambda(o,fedAcc(scala.Predef.Map((x, o)))(f(x))(λ(otype)(x => x))), C(y))))
  }

}