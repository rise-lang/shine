package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.primitives.imperative.AsScalarAcc
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class AsScalar(
  n: Nat,
  m: Nat,
  dt: ScalarType,
  access: AccessType,
  array: Phrase[ExpType]
) extends ExpPrimitive {

  array :: expT(n `.` vec(m, dt), access)
  override val t: ExpType = expT((n * m) `.` dt, access)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    AsScalar(fun.nat(n), fun.nat(m), fun.data(dt), fun.access(access),
      VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = s"(asScalar ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <asScalar n={ToString(n)} access={ToString(access)}>
      {Phrases.xmlPrinter(array)}
    </asScalar>

  override def fedeTranslation(
    env: Predef.Map[Identifier[ExpType],
      Identifier[AccType]]
  )(
    C: Phrase[AccType ->: AccType]
  ): Phrase[AccType] = {
    import TranslationToImperative._
    fedAcc(env)(array)(fun(accT(C.t.inT.dataType))(o =>
      AsScalarAcc(n, m, dt, C(o))))
  }

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._
    acc(array)(AsScalarAcc(n, m, dt, A))
  }

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(array.t)(x => C(AsScalar(n, m, dt, access, x)) ))
  }
}
