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

import scala.xml.Elem


final case class Which(n: Nat, elemT: DataType, count: Nat, f: Phrase[ExpType ->: ExpType], input:Phrase[ExpType])
  extends ExpPrimitive {
  override val t = expT(count `.` IndexType(n), read)
  // Filter needs to allocate more memory than it's 'logical' type says

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    `new`(count `.` IndexType(n), output => {
      acceptorTranslation(output.wr) `;` C(output.rd)
    })
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(input)(λ(expT(ArrayType(n, elemT), read))(input => {
      comment("WHICH: counter") `;`
        `new`(IndexType(count), counter => {
          `for`(n, idx => {
            comment("WHICH: test local") `;`
              `new`(bool, testLocal => {
                acc(f(input `@` idx))(testLocal.wr) `;`
                  IfThenElse(testLocal.rd,
                    ((A `@` counter.rd) :=|IndexType(n)|idx) `;`
                      (counter.wr :=| IndexType(count) | NatAsIndex(count, IndexAsNat(count, counter.rd) + Natural(1)))
                    ,
                    Skip()
                  )
              })
          })
        })
    }
    ))
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"which"

  override def xmlPrinter: Elem = <which></which>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = Which(
    v.nat(n),
    v.data(elemT),
    v.nat(count),
    VisitAndRebuild(f, v),
    VisitAndRebuild(input, v)
  )
}

final case class ToDepArray(a: AccessType, inT: ArrayType, outT:DepArrayType, input: Phrase[ExpType]) extends ExpPrimitive {
  override val t = expT(outT, a)

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(input)(λ(expT(inT, a))(x => C(ToDepArray(a, inT, outT, x))))
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = ???

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"which"

  override def xmlPrinter: Elem = <which></which>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = ToDepArray(
    v.access(a),
    v.data(inT),
    v.data(outT),
    VisitAndRebuild(input, v)
  )
}
