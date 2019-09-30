package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}

import scala.xml.Elem

final case class PrintType(msg: String,
                           dt: DataType,
                           input: Phrase[ExpType])
  extends ExpPrimitive {

  println(s"$msg : $dt (DPIA level)")

  override val t: ExpType =
    (dt: DataType) ->:
      (input :: exp"[$dt, $read]") ->: exp"[$dt, $read]"


  override def eval(s: Store): Data = OperationalSemantics.eval(s, input)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    PrintType(msg, fun.data(dt), VisitAndRebuild(input, fun))
  }

  override def prettyPrint: String = s"printType(${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <printType dt={ToString(dt)}>
      <input type={ToString(ExpType(dt, read))}>
        {Phrases.xmlPrinter(input)}
      </input>
    </printType>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    acc(input)(A)
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(input)(C)
  }
}
