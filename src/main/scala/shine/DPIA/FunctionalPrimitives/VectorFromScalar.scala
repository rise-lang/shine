package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._

import scala.language.reflectiveCalls
import scala.xml.Elem

final case class VectorFromScalar(n: Nat,
                                  dt: ScalarType,
                                  arg: Phrase[ExpType])
  extends ExpPrimitive {

  arg :: expT(dt, read)
  override val t: ExpType = expT(vec(n, dt), read)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    VectorFromScalar(f.nat(n), f.data(dt), VisitAndRebuild(arg, f))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = s"(makeVector ${n.toString} ${PrettyPhrasePrinter(arg)})"

  override def xmlPrinter: Elem =
    <makeVector n={ToString(n)}>
      {Phrases.xmlPrinter(arg)}
    </makeVector>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(arg)(λ(expT(dt, read))(e => A :=|VectorType(n, dt)| VectorFromScalar(n, dt, e) ))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(arg)(λ(expT(dt, read))(e => C(VectorFromScalar(n, dt, e)) ))
  }
}
