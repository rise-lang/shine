package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL.{λ, _}
import idealised.DPIA.ImperativePrimitives.TakeAcc
import idealised.DPIA.Phrases.{ExpPrimitive, Phrase, VisitAndRebuild}
import idealised.DPIA.Semantics.OperationalSemantics.{Data, Store}
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType, _}
import idealised.DPIA.{->, Nat, Phrases, _}

import scala.language.reflectiveCalls
import scala.xml.Elem

// this takes n many elements from an array of m elements
final case class Take(n: Nat,
                      m: Nat,
                      dt: DataType,
                      array: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (n: Nat) -> (m: Nat) -> (dt: DataType) ->
      (array :: exp"[${n + m}.$dt]") -> exp"[$n.$dt]"

  override def eval(s: Store): Data = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Take(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._
    ???
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommandType] =
    ???

  override def continuationTranslation(C: Phrase[->[ExpType, CommandType]])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._
    con(array)(λ(exp"[${n + m}.$dt]")(x => C(Take(n, m, dt, x))))
  }

  override def xmlPrinter: Elem =
    <take n={n.toString} m={m.toString} dt={dt.toString}>
      {Phrases.xmlPrinter(array)}
    </take>

  override def prettyPrint: String = s"(take $array)"

}
