package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types.{AccType, AccessType, CommType, DataType, ExpType}
import shine.DPIA.{->:, Phrases, expT}

import scala.xml.Elem

final case class PrintType(msg: String,
                           dt: DataType,
                           access: AccessType,
                           input: Phrase[ExpType]
                          ) extends ExpPrimitive {

  println(s"$msg : $dt (DPIA level)")

  input :: expT(dt, access)
  override val t: ExpType = expT(dt, access)

  override def eval(s: Store): Data = OperationalSemantics.eval(s, input)

  override def visitAndRebuild(
                                fun: VisitAndRebuild.Visitor
                              ): Phrase[ExpType] = {
    PrintType(msg, fun.data(dt), fun.access(access),
      VisitAndRebuild(input, fun))
  }

  override def prettyPrint: String = s"printType(${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <printType dt={ToString(dt)} access={ToString(access)}>
      <input type={ToString(ExpType(dt, access))}>
        {Phrases.xmlPrinter(input)}
      </input>
    </printType>

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._
    acc(input)(A)
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._
    con(input)(C)
  }
}
