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
