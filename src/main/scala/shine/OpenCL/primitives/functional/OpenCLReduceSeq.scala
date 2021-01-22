package shine.OpenCL.primitives.functional

import shine.DPIA.Compilation._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._
import shine.OpenCL.primitives.intermediate.OpenCLReduceSeqI

import scala.xml.Elem

final case class OpenCLReduceSeq(
  n: Nat,
  initAddrSpace: shine.DPIA.Types.AddressSpace,
  dt1: DataType,
  dt2: DataType,
  f: Phrase[ExpType ->: ExpType ->: ExpType],
  init: Phrase[ExpType],
  array: Phrase[ExpType],
  unroll: Boolean
) extends ExpPrimitive {

  f :: expT(dt2, read) ->: expT(dt1, read) ->: expT(dt2, write)
  init :: expT(dt2, write)
  array :: expT(n`.`dt1, read)
  override val t: ExpType = expT(dt2, read)

  override def visitAndRebuild(
    fun: VisitAndRebuild.Visitor
  ): Phrase[ExpType] = {
    OpenCLReduceSeq(fun.nat(n), fun.addressSpace(initAddrSpace), fun.data(dt1),
      fun.data(dt2), VisitAndRebuild(f, fun), VisitAndRebuild(init, fun),
      VisitAndRebuild(array, fun), unroll)
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${initAddrSpace})" +
      s"(${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(init)})" +
      s"(${PrettyPhrasePrinter(array)})"

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(expT(n`.`dt1, read))(X =>
      OpenCLReduceSeqI(n, initAddrSpace, dt1, dt2,
        λ(expT(dt2, read))(x =>
          λ(expT(dt1, read))(y =>
            λ(accT(dt2))(o => acc( f(x)(y) )( o )))),
        init, X, C, unroll)(context)))
  }

  override def xmlPrinter: Elem =
    <reduce n={ToString(n)} addrSpace={ToString(initAddrSpace)}
            dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f type={ToString(
        ExpType(dt1, read) ->: (ExpType(dt2, read) ->: ExpType(dt2, write)))}>
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
      s"${Character.toLowerCase(name.charAt(0))}${name.substring(1)}"
    })

}
