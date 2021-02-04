package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.Skip
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.AdjustArraySizesForAllocations

import scala.xml.Elem

final case class Which(n: Nat, count: Nat, input:Phrase[ExpType])
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
    con(input)(λ(expT(ArrayType(n, bool), read))(input => {
      comment("WHICH: counter") `;`
        `new`(IndexType(count),
          counter => {
             (counter.wr :=|IndexType(count)| NatAsIndex(count, Natural(0)))`;`
             `for`(n, idx => {
                IfThenElse(input `@` idx,
                  ((A `@` counter.rd) :=|IndexType(n)|idx) `;`
                    (counter.wr :=| IndexType(count) | NatAsIndex(count, IndexAsNat(count, counter.rd) + Natural(1))),
                  Skip()
                )
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
    v.nat(count),
    VisitAndRebuild(input, v)
  )
}



final case class OclWhich(n: Nat, count: Nat, input:Phrase[ExpType])
  extends ExpPrimitive {
  override val t = expT(count `.` IndexType(n), write)

  // Filter needs to allocate more memory than it's 'logical' type says

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = ???

  /**
    * Continuation tranlsation would allocate. If you really want to, call oclToMem by yourself
    * @param A
    * @param context
    * @return
    */

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(input)(λ(expT(ArrayType(n, bool), read))(input => {
      comment("WHICH: counter") `;`
        shine.OpenCL.DSL.`new`(AddressSpace.Private)(int,
          counter => {
            (counter.wr :=|int| toLiteralInt(0))`;`
              `for`(n, idx => {
                IfThenElse(input `@` idx,
                  ((A `@` Transmute(read, int, IndexType(count), counter.rd)) :=|IndexType(n)| idx) `;`
                    (counter.wr :=| int | counter.rd + toLiteralInt(1)),
                  Skip()
                )
              })
          })
    }
    ))
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"which"

  override def xmlPrinter: Elem = <which></which>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = OclWhich(
    v.nat(n),
    v.nat(count),
    VisitAndRebuild(input, v)
  )
}



final case class OclWhichMap(n: Nat, dt:DataType, count: Nat, input:Phrase[ExpType], f:Phrase[ExpType ->: ExpType])
  extends ExpPrimitive {
  override val t = expT(count `.` dt, write)

  // Filter needs to allocate more memory than it's 'logical' type says

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = ???

  /**
    * Continuation tranlsation would allocate. If you really want to, call oclToMem by yourself
    * @param A
    * @param context
    * @return
    */

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(input)(λ(expT(ArrayType(n, bool), read))(input => {
      comment("WHICH: counter") `;`
        shine.OpenCL.DSL.`new`(AddressSpace.Private)(int,
          counter => {
            (counter.wr :=|int| toLiteralInt(0))`;`
              `for`(n, idx => {
                IfThenElse(input `@` idx,
                  acc(f(idx))(A `@` Transmute(read, int, IndexType(count), counter.rd)) `;`
                    (counter.wr :=| int | counter.rd + toLiteralInt(1)),
                  Skip()
                )
              })
          })
    }
    ))
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"which"

  override def xmlPrinter: Elem = <which></which>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = OclWhichMap(
    v.nat(n),
    v.data(dt),
    v.nat(count),
    VisitAndRebuild(input, v),
    VisitAndRebuild(f, v)
  )
}
