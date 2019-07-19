package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL.{`new`=> _, _}
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.{Data, Store}
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}
import idealised.OpenCL.AddressSpace
import idealised.OpenCL.DSL._

import scala.xml.Elem

abstract class To(dt1: DataType,
                  dt2: DataType,
                  f: Phrase[ExpType ->: ExpType],
                  input: Phrase[ExpType],
                  addressSpace: AddressSpace,
                  private val makeTo: (DataType, DataType,
                    Phrase[ExpType ->: ExpType], Phrase[ExpType]) => To)
  extends ExpPrimitive {

  override val t: ExpType =
    (dt1: DataType) -> (dt2: DataType) ->
      (f :: t"exp[$dt1] -> exp[$dt2]") ->
        (input :: exp"[$dt1]") ->
          (addressSpace : AddressSpace) -> exp"[$dt2]"

  override def eval(s: Store): Data = OperationalSemantics.eval(s, input)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    makeTo(fun.data(dt1), fun.data(dt2), VisitAndRebuild(f, fun), VisitAndRebuild(input, fun))
  }

  override def prettyPrint: String =
    s"(to$addressSpace ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <to dt1={ToString(dt1)} dt2={ToString(dt2)} addressSpace={ToString(addressSpace)}>
      <f type={ToString(ExpType(dt1) -> ExpType(dt2))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(dt1))}>
        {Phrases.xmlPrinter(input)}
      </input>
    </to>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(this)(λ( exp"[$dt2]" )(x => acc(x)(A) ))
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(this)(λ( exp"[$dt2]" )(x => mapAcc(f, x)(A) ))
  }

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    newWithAddrSpace(dt2, addressSpace, tmp => acc(f(input))(tmp.wr) `;` C(tmp.rd) )
  }
}
