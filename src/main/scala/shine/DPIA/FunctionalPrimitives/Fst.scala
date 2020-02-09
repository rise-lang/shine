package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._

import scala.language.reflectiveCalls
import scala.xml.Elem

final case class Fst(
  dt1: DataType,
  dt2: DataType,
  access: AccessType,
  pair: Phrase[ExpType]
) extends ExpPrimitive {

  //FIXME this should be polymorphic over the access type
  override val t: ExpType =
    (dt1: DataType) ->: (dt2: DataType) ->: (access: AccessType) ->:
      (pair :: exp"[$dt1 x $dt2, $access]") ->: exp"[$dt1, $access]"

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, pair) match {
      case r: PairData => r.fst
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(
    fun: VisitAndRebuild.Visitor
  ): Phrase[ExpType] = {
    Fst(fun.data(dt1), fun.data(dt2), fun.access(access),
      VisitAndRebuild(pair, fun))
  }

  override def prettyPrint: String = s"${PrettyPhrasePrinter(pair)}._1"

  override def xmlPrinter: Elem =
    <fst dt1={ToString(dt1)} dt2={ToString(dt2)} access={ToString(access)}>
      {Phrases.xmlPrinter(pair)}
    </fst>

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._

    //TODO Assignments for general types should not be allowed, making this definition invalid
    dt1 match { case _ : BasicType => ; case _ => error(s"$dt1 assign") }
    con(pair)(λ(exp"[$dt1 x $dt2, $read]")(x =>
      A :=|dt1| Fst(dt1, dt2, access, x)) )
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._
    con(pair)(λ(exp"[$dt1 x $dt2, $read]")(x => C(Fst(dt1, dt2, access, x))))
  }
}
