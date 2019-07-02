package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL.{λ, _}
import idealised.DPIA.Phrases.{ExpPrimitive, Phrase, VisitAndRebuild}
import idealised.DPIA.Semantics.OperationalSemantics.{Data, Store}
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType, _}
import idealised.DPIA.{->, Nat, Phrases, _}

import scala.language.reflectiveCalls
import scala.xml.Elem

// TODO: invalid for empty array
final case class PadClamp(n: Nat,
                          l: Nat,
                          r: Nat,
                          dt: DataType,
                          array: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (n: Nat) -> (l: Nat) -> (r: Nat) -> (dt: DataType) ->
      (array :: exp"[$n.$dt, $read]") -> exp"[${l + n + r}.$dt, $read]"

  override def eval(s: Store): Data = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    PadClamp(fun(n), fun(l), fun(r), fun(dt), VisitAndRebuild(array, fun))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] =
    ???

  override def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommandType] =
    ???

  override def continuationTranslation(C: Phrase[->[ExpType, CommandType]])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._
    con(array)(λ(exp"[$n.$dt, $read]")(x => C(PadClamp(n, l, r, dt, x))))
  }

  override def xmlPrinter: Elem =
    <padClamp n={n.toString} l={l.toString} r={r.toString} dt={dt.toString}>
      {Phrases.xmlPrinter(array)}
    </padClamp>

  override def prettyPrint: String = s"(padClamp $array)"
}
