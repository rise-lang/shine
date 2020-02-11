package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.MapSndAcc
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

final case class MapSnd(dt1: DataType,
                        dt2: DataType,
                        dt3: DataType,
                        f: Phrase[ExpType ->: ExpType],
                        record: Phrase[ExpType]) extends ExpPrimitive
{

  f :: expT(dt2, read) ->: expT(dt3, read)
  record :: expT(dt1 x dt2, read)
  override val t: ExpType = expT(dt1 x dt3, read)

  override def eval(s: Store): Data = {
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, record) match {
      case r: PairData => PairData(r.fst, OperationalSemantics.eval(s, fE(Literal(r.snd))))
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    MapSnd(fun.data(dt1), fun.data(dt2), fun.data(dt3),
      VisitAndRebuild(f, fun),
      VisitAndRebuild(record, fun))
  }

  override def prettyPrint: String = s"(mapSnd ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(record)})"

  override def xmlPrinter: Elem =
    <mapSnd dt1={ToString(dt1)} dt2={ToString(dt2)} dt3={ToString(dt3)}>
      <f>{Phrases.xmlPrinter(f)}</f>
      <record>{Phrases.xmlPrinter(record)}</record>
    </mapSnd>

  override def fedeTranslation(env: scala.Predef.Map[Identifier[ExpType], Identifier[AccType]])
                              (C: Phrase[AccType ->: AccType]) : Phrase[AccType] = {
    import TranslationToImperative._

    val x = Identifier(freshName("fede_x"), ExpType(dt2, read))

    val otype = AccType(dt3)
    val o = Identifier(freshName("fede_o"), otype)

    fedAcc(env)(record)(fun(env.toList.head._2.t)(y =>
      MapSndAcc(dt1, dt2, dt3,
        Lambda(o, fedAcc(scala.Predef.Map(x -> o))(f(x))(fun(otype)(x => x))),
        C(y))))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    val x = Identifier(freshName("fede_x"), ExpType(dt2, read))

    val otype = AccType(dt3)
    val o = Identifier(freshName("fede_o"), otype)

    acc(record)(MapSndAcc(dt1, dt2, dt3,
      Lambda(o, fedAcc(scala.Predef.Map(x -> o))(f(x))(fun(otype)(x => x))),
      A))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    // assumption: f does not need to be translated, it does indexing only
    con(record)(fun(record.t)(x => C(MapSnd(dt1, dt2, dt3, f, x))))
  }
}
