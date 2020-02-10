package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.{ArrayData, Data, Store}
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

// cycles on the m elements of an array (modulo indexing) to produce an array of n elements
final case class Cycle(n: Nat,
                       m: Nat,
                       dt: DataType,
                       input: Phrase[ExpType])
  extends ExpPrimitive
{
  input :: expT(m`.`dt, read)
  override val t: ExpType = expT(n`.`dt, read)

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, input) match {
      case ArrayData(a) =>
        val N = n.eval
        ArrayData(Stream.continually(a).flatten.take(N).toVector)
      case _ => throw new Exception("this should not happen")
    }
  }

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] =
    Cycle(v.nat(n), v.nat(m), v.data(dt), VisitAndRebuild(input, v))

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(input)(fun(expT(m`.`dt, read))(x => C(Cycle(n, m, dt, x))))
  }

  override def xmlPrinter: xml.Elem =
    <cycle n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(input)}
    </cycle>

  override def prettyPrint: String = s"(cycle $input)"
}
