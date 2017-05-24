package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Semantics.OperationalSemantics.{Data, Store}
import idealised.DPIA.Compilation.RewriteToImperative
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA.{Phrases, _}
import idealised.DPIA.DSL._
import idealised.OpenCL.AddressSpace

import scala.xml.Elem

abstract class To(dt1: DataType,
                  dt2: DataType,
                  f: Phrase[ExpType -> ExpType],
                  input: Phrase[ExpType],
                  addressSpace: AddressSpace,
                  private val makeTo: (DataType, DataType,
                    Phrase[ExpType -> ExpType], Phrase[ExpType]) => To)
  extends ExpPrimitive {

  override lazy val `type` = exp"[$dt2]"

  override def typeCheck(): Unit = {
    import idealised.DPIA.Types.TypeChecker._
    (dt1: DataType) -> (dt2: DataType) ->
      (f :: t"exp[$dt1] -> exp[$dt2]") ->
      (input :: exp"[$dt1]") ->
      `type`
  }

  override def eval(s: Store): Data = OperationalSemantics.eval(s, input)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    makeTo(fun(dt1), fun(dt2), VisitAndRebuild(f, fun), VisitAndRebuild(input, fun))
  }

  override def prettyPrint: String =
    s"(to$addressSpace ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <to dt1={ToString(dt1)} dt2={ToString(dt2)}>
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

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    assert(dt1 != null && dt2 != null)
    import RewriteToImperative._

    con(this)(λ( exp"[$dt2]" )(x => acc(x)(A) ))
  }

  override def continuationTranslation(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    assert(dt1 != null && dt2 != null)
    import RewriteToImperative._

    `new`(dt2, addressSpace, tmp => acc(f(input))(tmp.wr) `;` C(tmp.rd) )
  }
}
