package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem



final case class LiftNats(n:Nat, a: AccessType, outT: DataType, input: Phrase[ExpType], f: Phrase[`(natCollection)->:`[ExpType]])
  extends ExpPrimitive {
  override val t = expT(outT, a)

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(input)(λ(expT(n `.` NatType, read))(input => {
      LiftNatsI(n, input, _Λ_[NatCollectionKind]()((i: NatCollectionIdentifier) => con(f(i))(C)))
    }))
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(input)(λ(expT(n `.` NatType, read))(input => {
      LiftNatsI(n, input, _Λ_[NatCollectionKind]()((i: NatCollectionIdentifier) => acc(f(i))(A)))
    }))
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"liftNats"

  override def xmlPrinter: Elem = <liftNats></liftNats>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): LiftNats = LiftNats(
    v.nat(n),
    v.access(a),
    v.data(outT),
    VisitAndRebuild(input, v),
    VisitAndRebuild(f, v)
  )
}

final case class LiftNatsI(n:Nat, input: Phrase[ExpType], f:Phrase[`(natCollection)->:`[CommType]]) extends CommandPrimitive {
  override val t = comm

  override def eval(s: Store): Store = ???

  override def prettyPrint: String = s"liftNatsI"

  override def xmlPrinter: Elem = <liftNatsI></liftNatsI>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): LiftNatsI = LiftNatsI(
    v.nat(n),
    VisitAndRebuild(input, v),
    VisitAndRebuild(f, v)
  )
}
