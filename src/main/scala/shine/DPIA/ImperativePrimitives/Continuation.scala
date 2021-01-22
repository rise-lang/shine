package shine.DPIA.ImperativePrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class Continuation(dt: DataType,
                              cont: Phrase[(ExpType ->: CommType) ->: CommType])
  extends ExpPrimitive
{
  cont :: (expT(dt, read) ->: comm) ->: comm
  override val t: ExpType = expT(dt, read)

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Continuation(v.data(dt), VisitAndRebuild(cont, v))
  }

  override def eval(s: Store): Data = ???

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] =
    throw new Exception("This should not happen")

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] =
    throw new Exception("This should not happen")

  override def prettyPrint: String = s"(continuation $cont)"

  override def xmlPrinter: Elem =
    <continuation dt={ToString(dt)}>
        {Phrases.xmlPrinter(cont)}
    </continuation>
}