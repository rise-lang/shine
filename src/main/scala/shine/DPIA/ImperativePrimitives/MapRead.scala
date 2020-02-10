package shine.DPIA.ImperativePrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

final case class MapRead(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         f: Phrase[ExpType ->: (ExpType ->: CommType) ->: CommType],
                         input: Phrase[ExpType])
  extends ExpPrimitive
{
  f :: expT(dt1, read) ->: (expT(dt2, read) ->: comm) ->: comm
  input :: expT(n`.`dt1, read)
  override val t: ExpType = expT(n`.`dt2, read)

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    MapRead(v.nat(n), v.data(dt1), v.data(dt2), VisitAndRebuild(f, v), VisitAndRebuild(input, v))
  }

  override def eval(s: Store): Data = ???

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] =
    throw new Exception("This should not happen")

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] =
    throw new Exception("This should not happen")

  override def prettyPrint: String = s"(mapRead $f $input)"

  override def xmlPrinter: xml.Elem =
    <mapRead n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f>
        {Phrases.xmlPrinter(f)}
      </f>
      <input>
        {Phrases.xmlPrinter(input)}
      </input>
    </mapRead>
}
