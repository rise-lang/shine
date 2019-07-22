package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA._
import idealised.DPIA.Types._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._

import scala.language.reflectiveCalls

final case class MapRead(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         f: Phrase[ExpType ->: (ExpType ->: CommType) ->: CommType],
                         input: Phrase[ExpType])
  extends ExpPrimitive
{
  override val t: ExpType =
    (n: Nat) ->: (dt1: DataType) ->: (dt2: DataType) ->:
      (f :: exp"[$dt1, $read]" ->: ((exp"[$dt2, $read]" ->: comm) ->: comm)) ->:
      (input :: exp"[$n.$dt1, $read]") ->: exp"[$n.$dt2, $read]"

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    MapRead(v.nat(n), v.data(dt1), v.data(dt2), VisitAndRebuild(f, v), VisitAndRebuild(input, v))
  }

  override def eval(s: Store): Data = ???

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] =
    throw new Exception("This should not happen")


  override def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
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
