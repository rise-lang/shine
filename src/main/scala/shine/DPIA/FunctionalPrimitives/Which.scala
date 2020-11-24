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

final case class Transmute(a: AccessType, inT: DataType, outT:DataType, input: Phrase[ExpType]) extends ExpPrimitive {
  override val t = expT(outT, a)

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(input)(λ(expT(inT, a))(x => C(Transmute(a, inT, outT, x))))
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    acc(input)(TransmuteAcc(inT, outT, A))
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"transmute"

  override def xmlPrinter: Elem = <transmute></transmute>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = Transmute(
    v.access(a),
    v.data(inT),
    v.data(outT),
    VisitAndRebuild(input, v)
  )
}


final case class TransmuteAcc(inT: DataType, outT:DataType, A:Phrase[AccType]) extends AccPrimitive {
  override val t = accT(inT)


  override def prettyPrint: String = s"transmuteAcc"

  override def xmlPrinter: Elem = <transmuteAcc></transmuteAcc>

  override def eval(s: Store): OperationalSemantics.AccIdentifier = ???

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): TransmuteAcc =
    TransmuteAcc(v.data(inT), v.data(outT), VisitAndRebuild(A, v))
}