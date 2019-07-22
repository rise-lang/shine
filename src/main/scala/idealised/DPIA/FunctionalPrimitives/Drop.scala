package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL.{λ, _}
import idealised.DPIA.Phrases.{ExpPrimitive, Phrase, VisitAndRebuild}
import idealised.DPIA.Semantics.OperationalSemantics.{Data, Store}
import idealised.DPIA.Types.{AccType, CommType, DataType, ExpType, _}
import idealised.DPIA.{->:, Nat, Phrases, _}

import scala.language.reflectiveCalls
import scala.xml.Elem

// this drops n many elements from an array of n + m elements
final case class Drop(n: Nat,
                      m: Nat,
                      w: AccessType,
                      dt: DataType,
                      array: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (n: Nat) ->: (m: Nat) ->: (w: AccessType) ->: (dt: DataType) ->:
      (array :: exp"[${n + m}.$dt, $w]") ->: exp"[$m.$dt, $w]"

  override def eval(s: Store): Data = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Drop(fun.nat(n), fun.nat(m), w, fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    //TODO if this is not needed then the AccessType param is not needed either
    ???
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(exp"[${n + m}.$dt, $read]")(x => C(Drop(n, m, read, dt, x))))
  }

  override def xmlPrinter: Elem =
    <drop n={n.toString} m={m.toString} w={w.toString} dt={dt.toString}>
      {Phrases.xmlPrinter(array)}
    </drop>

  override def prettyPrint: String = s"(drop $array)"

}
