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

final case class AsVector(
  n: Nat,
  m: Nat,
  dt: ScalarType,
  access: AccessType,
  array: Phrase[ExpType]
) extends ExpPrimitive {

  override val t: ExpType =
    (n: Nat) ~>: (m: Nat) ~>: (dt: ScalarType) ~>:
      (array :: expT({m * n}`.`dt, access)) ~>:
        expT(m`.`vec(n, dt), access)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    AsVector(f.nat(n), f.nat(m), f.data(dt), f.access(access),
      VisitAndRebuild(array, f))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"(asVector ${n.toString} ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <asVector n={ToString(n)} dt={ToString(dt)} access={ToString(access)}>
      {Phrases.xmlPrinter(array)}
    </asVector>

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._

    acc(array)(AsVectorAcc(n, m, dt, A))
  }

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    TranslationToImperative.con(array)(λ(array.t)(x =>
      C(AsVector(n, m, dt, access, x)) ))
  }
}
