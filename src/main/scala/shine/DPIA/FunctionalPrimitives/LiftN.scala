package shine.DPIA.FunctionalPrimitives


import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem



final case class LiftN(a: AccessType, outT: DataType, input: Phrase[ExpType], f: Phrase[`(nat)->:`[ExpType]])
  extends ExpPrimitive {
  override val t = expT(outT, a)

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(input)(λ(expT(NatType, read))(input => {
      LiftNI(input, _Λ_[NatKind]()((i: NatIdentifier) => con(f(i))(C)))
    }))
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(input)(λ(expT(NatType, read))(input => {
      LiftNI(input, _Λ_[NatKind]()((i: NatIdentifier) => acc(f(i))(A)))
    }))
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"liftN"

  override def xmlPrinter: Elem = <liftN></liftN>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): LiftN = LiftN(
    v.access(a),
    v.data(outT),
    VisitAndRebuild(input, v),
    VisitAndRebuild(f, v)
  )
}

final case class LiftNI(input: Phrase[ExpType], f:Phrase[`(nat)->:`[CommType]]) extends CommandPrimitive {
  override val t = comm

  override def eval(s: Store): Store = ???

  override def prettyPrint: String = s"liftN"

  override def xmlPrinter: Elem = <liftN></liftN>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): LiftNI = LiftNI(
    VisitAndRebuild(input, v),
    VisitAndRebuild(f, v)
  )
}
