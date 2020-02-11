package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.{MapAcc, MapRead}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

final case class Map(override val n: Nat,
                     override val dt1: DataType,
                     override val dt2: DataType,
                     override val f: Phrase[ExpType ->: ExpType],
                     override val array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array)
{
  override def makeMap: (Nat, DataType, DataType, Phrase[ExpType ->: ExpType], Phrase[ExpType]) => AbstractMap = Map

  override def fedeTranslation(env: scala.Predef.Map[Identifier[ExpType], Identifier[AccType]])
                              (C: Phrase[AccType ->: AccType]) : Phrase[AccType] = {
    import TranslationToImperative._

    val x = Identifier(freshName("fede_x"), ExpType(dt1, read))

    val otype = AccType(dt2)
    val o = Identifier(freshName("fede_o"), otype)

    fedAcc(env)(array)(λ(env.toList.head._2.t)(y =>
      MapAcc(n, dt2, dt1,
        Lambda(o,fedAcc(scala.Predef.Map((x, o)))(f(x))(λ(otype)(x => x))), C(y))))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    val x = Identifier(freshName("fede_x"), ExpType(dt1, read))

    val otype = AccType(dt2)
    val o = Identifier(freshName("fede_o"), otype)

    acc(array)(MapAcc(n, dt2, dt1,
      Lambda(o,fedAcc(scala.Predef.Map((x, o)))(f(x))(λ(otype)(x => x))),
      A))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(expT(n`.`dt1, read))(x =>
      C(MapRead(n, dt1, dt2,
        fun(expT(dt1, read))(a =>
          fun(expT(dt2, read) ->: (comm: CommType))(cont =>
            con(f(a))(fun(expT(dt2, read))(b => Apply(cont, b))))),
        x))))
  }
}
