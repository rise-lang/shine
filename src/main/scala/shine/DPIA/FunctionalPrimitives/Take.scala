package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL.{λ, _}
import shine.DPIA.Phrases.{ExpPrimitive, Phrase, VisitAndRebuild}
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, _}
import shine.DPIA.Types.DataType._
import shine.DPIA.{->:, Nat, Phrases, _}

import scala.xml.Elem

// this takes n many elements from an array of n + m elements
final case class Take(n: Nat,
                      m: Nat,
                      w: AccessType,
                      dt: DataType,
                      array: Phrase[ExpType])
  extends ExpPrimitive {

  array :: expT({n + m}`.`dt, w)
  override val t: ExpType = expT(n`.`dt, w)

  override def eval(s: Store): Data = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Take(fun.nat(n), fun.nat(m), fun.access(w), fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    ???
  }

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(expT({n + m}`.`dt, read))(x => C(Take(n, m, w, dt, x))))
  }

  override def xmlPrinter: Elem =
    <take n={n.toString} m={m.toString} dt={dt.toString}>
      {Phrases.xmlPrinter(array)}
    </take>

  override def prettyPrint: String = s"(take $array)"

}
