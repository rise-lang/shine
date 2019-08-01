package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA.DSL._
import idealised.DPIA._
import idealised.OpenCL.IntermediatePrimitives.OpenCLReduceSeqI
import idealised.DPIA.Semantics.OperationalSemantics._

import scala.xml.Elem

final case class OpenCLReduceSeq(n: Nat,
                                 initAddrSpace: idealised.DPIA.Types.AddressSpace,
                                 dt1: DataType,
                                 dt2: DataType,
                                 f: Phrase[ExpType ->: ExpType ->: ExpType],
                                 init: Phrase[ExpType],
                                 array: Phrase[ExpType])
  extends ExpPrimitive
{
  override val t: ExpType =
    (n: Nat) ->: (dt1: DataType) ->: (dt2: DataType) ->:
      (f :: t"exp[$dt1, $read] -> exp[$dt2, $read] -> exp[$dt2, $read]") ->:
      (init :: exp"[$dt2, $read]") ->: (initAddrSpace : AddressSpace) ->:
      (array :: exp"[$n.$dt1, $read]") ->: exp"[$dt2, $read]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    OpenCLReduceSeq(fun.nat(n), fun.addressSpace(initAddrSpace), fun.data(dt1), fun.data(dt2),
      VisitAndRebuild(f, fun), VisitAndRebuild(init, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${initAddrSpace}) (${PrettyPhrasePrinter(f)}) " +
      s"(${PrettyPhrasePrinter(init)}) (${PrettyPhrasePrinter(array)})"

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$n.$dt1, $read]")(X =>
      con(init)(λ(exp"[$dt2, $write]")(Y =>
        OpenCLReduceSeqI(n, initAddrSpace, dt1, dt2,
          λ(exp"[$dt1, $read]")(x => λ(exp"[$dt2, $read]")(y => λ(acc"[$dt2]")(o => acc( f(x)(y) )( o )))),
          //TODO acceptor takes r which should be Write
          Y, X, λ(exp"[$dt2, $read]")(r => acc(r)(A)))(context)))))

  }

  override def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$n.$dt1, $read]")(X =>
      con(init)(λ(exp"[$dt2, $write]")(Y =>
        OpenCLReduceSeqI(n, initAddrSpace, dt1, dt2,
          λ(exp"[$dt1]")(x => λ(exp"[$dt2]")(y => λ(acc"[$dt2]")(o => acc( f(x)(y) )( o )))),
          Y, X, C)(context)))))
  }

  override def xmlPrinter: Elem =
    <reduce n={ToString(n)} addrSpace={ToString(initAddrSpace)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f type={ToString(ExpType(dt1, read) ->: (ExpType(dt2, read) ->: ExpType(dt2, write)))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <init type={ToString(ExpType(dt2, write))}>
        {Phrases.xmlPrinter(init)}
      </init>
      <input type={ToString(ExpType(ArrayType(n, dt1), read))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </reduce>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })

}
