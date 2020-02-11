package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL.{λ, _}
import shine.DPIA.Phrases.{ExpPrimitive, Phrase, VisitAndRebuild}
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, _}
import shine.DPIA.Types.DataType._
import shine.DPIA.{->:, Nat, Phrases, _}

import scala.xml.Elem

// TODO: invalid for empty array
final case class PadClamp(n: Nat,
                          l: Nat,
                          r: Nat,
                          dt: DataType,
                          array: Phrase[ExpType])
  extends ExpPrimitive {

  array :: expT(n`.`dt, read)
  override val t: ExpType = expT({l + n + r}`.`dt, read)

  override def eval(s: Store): Data = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    PadClamp(fun.nat(n), fun.nat(l), fun.nat(r), fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(expT(n`.`dt, read))(x => C(PadClamp(n, l, r, dt, x))))
  }

  override def xmlPrinter: Elem =
    <padClamp n={n.toString} l={l.toString} r={r.toString} dt={dt.toString}>
      {Phrases.xmlPrinter(array)}
    </padClamp>

  override def prettyPrint: String = s"(padClamp $array)"
}
