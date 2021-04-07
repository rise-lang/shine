package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.imperative.MapSndAcc
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class MapSnd(w: AccessType,
                        dt1: DataType,
                        dt2: DataType,
                        dt3: DataType,
                        f: Phrase[ExpType ->: ExpType],
                        record: Phrase[ExpType]
                       ) extends ExpPrimitive with AccT with FedeT {
  f :: expT(dt2, w) ->: expT(dt3, w)
  record :: expT(dt1 x dt2, w)
  override val t: ExpType = expT(dt1 x dt3, w)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] = {
    val x = Identifier(freshName("fede_x"), ExpType(dt2, write))

    val otype = AccType(dt3)
    val o = Identifier(freshName("fede_o"), otype)

    acc(record)(MapSndAcc(dt1, dt2, dt3,
      Lambda(o, fedAcc(scala.Predef.Map(x -> o))(f(x))(fun(otype)(x => x))),
      A))
  }

  def fedeTranslation(env: Predef.Map[Identifier[ExpType], Identifier[AccType]])
                     (C: Phrase[AccType ->: AccType]): Phrase[AccType] = {
    val x = Identifier(freshName("fede_x"), ExpType(dt2, read))

    val otype = AccType(dt3)
    val o = Identifier(freshName("fede_o"), otype)

    fedAcc(env)(record)(fun(env.toList.head._2.t)(y =>
      MapSndAcc(dt1, dt2, dt3,
        Lambda(o, fedAcc(scala.Predef.Map(x -> o))(f(x))(fun(otype)(x => x))),
        C(y))))
  }

  override def eval(s: Store): Data = {
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, record) match {
      case r: PairData =>
        PairData(r.fst, OperationalSemantics.eval(s, fE(Literal(r.snd))))
      case _ => throw new Exception("This should not happen")
    }
  }
}
