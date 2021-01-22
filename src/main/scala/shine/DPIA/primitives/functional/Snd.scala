package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

final case class Snd(
  dt1: DataType,
  dt2: DataType,
  pair: Phrase[ExpType]
) extends ExpPrimitive {

  pair :: expT(dt1 x dt2, read)
  override val t: ExpType = expT(dt2, read)

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, pair) match {
      case r: PairData => r.snd
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Snd(f.data(dt1), f.data(dt2), VisitAndRebuild(pair, f))
  }

  override def xmlPrinter: Elem =
    <snd dt1={ToString(dt1)} dt2={ToString(dt2)}>
      {Phrases.xmlPrinter(pair)}
    </snd>

  override def prettyPrint: String = s"${PrettyPhrasePrinter(pair)}._2"

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._
    con(pair)(λ(expT(dt1 x dt2, read))(x => C(Snd(dt1, dt2, x))))
  }
}
