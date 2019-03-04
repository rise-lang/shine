package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA.DSL._
import idealised.DPIA._
import idealised.OpenCL.AddressSpace
import idealised.OpenCL.IntermediatePrimitives.OpenCLReduceSeqI
import idealised.DPIA.Semantics.OperationalSemantics._

import scala.xml.Elem

final case class OpenCLReduceSeq(n: Nat,
                           dt1: DataType,
                           dt2: DataType,
                           f: Phrase[ExpType -> (ExpType -> ExpType)],
                           init: Phrase[ExpType],
                           initAddrSpace: AddressSpace,
                           array: Phrase[ExpType])
  extends ExpPrimitive
{
  override val `type`: ExpType =
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (f :: t"exp[$dt1] -> exp[$dt2] -> exp[$dt2]") ->
      (init :: exp"[$dt2]") -> (initAddrSpace : AddressSpace) ->
      (array :: exp"[$n.$dt1]") -> exp"[$dt2]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    OpenCLReduceSeq(fun(n), fun(dt1), fun(dt2),
      VisitAndRebuild(f, fun), VisitAndRebuild(init, fun), initAddrSpace, VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)}) " +
      s"(${PrettyPhrasePrinter(init)}) (${PrettyPhrasePrinter(array)})"

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(this)(λ(exp"[$dt2]")(r => acc(r)(AccExt(A))))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$n.$dt1]")(X =>
      con(init)(λ(exp"[$dt2]")(Y =>
        OpenCLReduceSeqI(n, dt1, dt2,
          λ(exp"[$dt1]")(x => λ(exp"[$dt2]")(y => λ(acc"[$dt2]")(o => acc( f(x)(y) )( AccExt(o) )))),
          Y, initAddrSpace, X, C)(context)))))
  }

  override def xmlPrinter: Elem =
    <reduce n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)} addressSpace={ToString(initAddrSpace)}>
      <f type={ToString(ExpType(dt1) -> (ExpType(dt2) -> ExpType(dt2)))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <init type={ToString(ExpType(dt2))}>
        {Phrases.xmlPrinter(init)}
      </init>
      <input type={ToString(ExpType(ArrayType(n, dt1)))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </reduce>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })

}
