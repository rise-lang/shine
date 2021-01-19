package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.Skip
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.Types.DataType._
import shine.OpenCL.FunctionalPrimitives.OpenCLReduceSeq

import scala.xml.Elem

final case class Count(n: Nat, input:Phrase[ExpType])
  extends ExpPrimitive {
  override val t = expT(IndexType(n), read)

  private val asReduction = {
    NatAsIndex(n, ReduceSeq(n, bool, NatType,
      λ(expT(NatType, read))(i =>
        λ(expT(bool, read))(b =>
          IfThenElse(b, i + Natural(1), i))
      ), Natural(0), input))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    asReduction.continuationTranslation(C)
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    asReduction.acceptorTranslation(A)
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"filter"

  override def xmlPrinter: Elem = <filter></filter>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = Count(
    v.nat(n),
    VisitAndRebuild(input, v)
  )
}


final case class OclCount(n: Nat, addressSpace: AddressSpace, input:Phrase[ExpType])
  extends ExpPrimitive {
  override val t = expT(IndexType(n), read)
  // Filter needs to allocate more memory than it's 'logical' type says

  val asReduction = {
    Transmute(read, int, IndexType(n), OpenCLReduceSeq(n, addressSpace, bool, int,
      λ(expT(int, read))(i =>
        λ(expT(bool, read))(b =>
          IfThenElse(b, i + toLiteralInt(1), i))
      ), toLiteralInt(0), input, unroll = false))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    this.asReduction.continuationTranslation(C)
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    this.asReduction.acceptorTranslation(A)
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"filter"

  override def xmlPrinter: Elem = <filter></filter>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = OclCount(
    v.nat(n),
    v.addressSpace(addressSpace),
    VisitAndRebuild(input, v)
  )
}
