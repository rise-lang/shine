package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.imperative.{MapAcc, MapRead}
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Map(n: Nat,
                     dt1: DataType,
                     dt2: DataType,
                     access: AccessType,
                     f: Phrase[ExpType ->: ExpType],
                     array: Phrase[ExpType]
                    ) extends ExpPrimitive with ContinuationTranslatable with AcceptorTranslatable {
  array :: expT(n`.`dt1, access)
  f :: expT(dt1, access) ->: expT(dt2, access)
  override val t: ExpType = expT(n`.`dt2, access)

  override def fedeTranslation(env: Predef.Map[Identifier[ExpType],Identifier[AccType]])
                              (C: Phrase[AccType ->: AccType]): Phrase[AccType] = {
    val x = Identifier(freshName("fede_x"), ExpType(dt1, access))

    val otype = AccType(dt2)
    val o = Identifier(freshName("fede_o"), otype)

    fedAcc(env)(array)(λ(env.toList.head._2.t)(y =>
      MapAcc(n, dt2, dt1,
        Lambda(o,
          fedAcc(Predef.Map((x, o)))(f(x))(λ(otype)(x => x))), C(y))))
  }

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] = {
    val x = Identifier(freshName("fede_x"), ExpType(dt1, write))

    val otype = AccType(dt2)
    val o = Identifier(freshName("fede_o"), otype)

    acc(array)(MapAcc(n, dt2, dt1,
      Lambda(o, fedAcc(scala.Predef.Map((x, o)))(f(x))(λ(otype)(x => x))),
      A))
  }

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(array)(λ(expT(n`.`dt1, read))(x =>
      C(MapRead(n, dt1, dt2,
        fun(expT(dt1, read))(a =>
          fun(expT(dt2, read) ->: (comm: CommType))(cont =>
            con(f(a))(fun(expT(dt2, read))(b => Apply(cont, b))))),
        x))))
}
