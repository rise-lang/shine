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

final case class Count(n: Nat, elemT: DataType, f: Phrase[ExpType ->: ExpType], input:Phrase[ExpType])
  extends ExpPrimitive {
  override val t = expT(IndexType(n), read)
  // Filter needs to allocate more memory than it's 'logical' type says

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    `new`(IndexType(n), output => {
      // We just newed it, so we know output is an identifier. We will need to play some tricks
      // here, and change it's type.
      acceptorTranslation(output.wr) `;` C(output.rd)
    })
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(input)(λ(expT(ArrayType(n, elemT), read))(input => {
      comment("COUNT: counter") `;`
      `new`(IndexType(n), counter => {
        `for`(n, idx => {
          comment("COUNT: test local") `;`
          `new`(bool, testLocal => {
            acc(f(input `@` idx))(testLocal.wr) `;`
              IfThenElse(testLocal.rd,
                counter.wr :=| IndexType(n) | NatAsIndex(n, IndexAsNat(n, counter.rd) + Natural(1)),
                Skip()
              )
          })
        }) `;`
          comment("COUNT: write out") `;`
        (A :=|IndexType(n)| counter.rd)
      })
    }
    ))
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"filter"

  override def xmlPrinter: Elem = <filter></filter>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = Count(
    v.nat(n),
    v.data(elemT),
    VisitAndRebuild(f, v),
    VisitAndRebuild(input, v)
  )
}

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
                (counter.wr :=| IndexType(count) | NatAsIndex(count, IndexAsNat(count, counter.rd) + Natural(1))) `;`
                  ((A `@` counter.rd) :=|IndexType(n)|idx)
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

  override def prettyPrint: String = s"filter"

  override def xmlPrinter: Elem = <filter></filter>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = Which(
    v.nat(n),
    v.data(elemT),
    v.nat(count),
    VisitAndRebuild(f, v),
    VisitAndRebuild(input, v)
  )
}

final case class LiftN(a: AccessType, outT: DataType, input: Phrase[ExpType], f: Phrase[`(nat)->:`[ExpType]])
  extends ExpPrimitive {
  override val t = expT(outT, a)

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(input)(λ(expT(NatType, read))(input => {
      LiftNI(input, _Λ_[NatKind]()((i: NatIdentifier) => con(f(i))(C)))
    }))
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = ???

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

