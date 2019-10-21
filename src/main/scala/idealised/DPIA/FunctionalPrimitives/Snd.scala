package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.language.reflectiveCalls
import scala.xml.Elem

final case class Snd(dt1: DataType,
                     dt2: DataType,
                     pair: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (dt1: DataType) ->: (dt2: DataType) ->:
      (pair :: exp"[$dt1 x $dt2, $read]") ->: exp"[$dt2, $read]"

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, pair) match {
      case r: PairData => r.snd
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Snd(f.data(dt1), f.data(dt2), VisitAndRebuild(pair, f))
  }

  override def xmlPrinter: Elem = <snd>
    {Phrases.xmlPrinter(pair)}
  </snd>

  override def prettyPrint: String = s"${PrettyPhrasePrinter(pair)}._2"

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    //TODO Assignments for general types should not be allowed, making this definition invalid
    assert(dt2 match { case _ : BasicType => true; case _ => false })
    con(pair)(λ(exp"[$dt1 x $dt2, $read]")(x => A :=|dt2| Snd(dt1, dt2, x)) )
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(pair)(λ(exp"[$dt1 x $dt2, $read]")(x => C(Snd(dt1, dt2, x)) ))
  }
}
