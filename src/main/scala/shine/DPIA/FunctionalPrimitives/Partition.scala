package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class Partition(n: Nat,
                           m: Nat,
                           lenF: NatToNat,
                           dt: DataType,
                           array: Phrase[ExpType])
  extends ExpPrimitive {

  array :: expT(n`.`dt, read)
  override val t: ExpType = expT(m`.d`{ i => lenF(i)`.`dt }, read)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Partition(fun.nat(n), fun.nat(m), fun.natToNat(lenF), fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = s"(partition $n $m $lenF ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <partition n={ToString(n)} m={ToString(m)} lenID={ToString(lenF)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </partition>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    ???
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(expT(n`.`dt, read))(x => C(Partition(n, m, lenF, dt, x)) ))
  }
}