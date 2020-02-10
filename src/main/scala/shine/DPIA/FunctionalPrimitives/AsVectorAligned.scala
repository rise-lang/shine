package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.AsVectorAcc
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class AsVectorAligned(n: Nat,
                                 m: Nat,
                                 dt: ScalarType,
                                 array: Phrase[ExpType])
  extends ExpPrimitive {

  array :: expT({m * n}`.`dt, read)
  override val t: ExpType = expT(m`.`vec(n, dt), read)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    AsVectorAligned(f.nat(n), f.nat(m), f.data(dt), VisitAndRebuild(array, f))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = s"(asVectorAligned ${n.toString} ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <asVectorAligned n={ToString(n)}>
      {Phrases.xmlPrinter(array)}
    </asVectorAligned>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    acc(array)(AsVectorAcc(n, m, dt, A))
  }

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    TranslationToImperative.con(array)(λ(array.t)(x => C(AsVectorAligned(n, m, dt, x)) ))
  }
}
