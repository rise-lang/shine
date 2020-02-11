package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL.{λ, _}
import shine.DPIA.Phrases.{ExpPrimitive, Phrase, VisitAndRebuild}
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, _}
import shine.DPIA.Types.DataType._
import shine.DPIA.{->:, Nat, Phrases, _}

import scala.xml.Elem

// this drops n many elements from an array of n + m elements
final case class Drop(n: Nat,
                      m: Nat,
                      w: AccessType,
                      dt: DataType,
                      array: Phrase[ExpType])
  extends ExpPrimitive {

  array :: expT({n + m}`.`dt, w)
  override val t: ExpType = expT(m`.`dt, w)

  override def eval(s: Store): Data = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Drop(fun.nat(n), fun.nat(m), w, fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    //TODO if this is not needed then the AccessType param is not needed either
    ???
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(expT({n + m}`.`dt, read))(x => C(Drop(n, m, read, dt, x))))
  }

  override def xmlPrinter: Elem =
    <drop n={n.toString} m={m.toString} w={w.toString} dt={dt.toString}>
      {Phrases.xmlPrinter(array)}
    </drop>

  override def prettyPrint: String = s"(drop $array)"

}
