package shine.OpenCL.FunctionalPrimitives


import shine.DPIA.Compilation._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.IntermediatePrimitives.{OpenCLScanSeqI, OpenCLScanSeqInclusiveI}

import scala.xml.Elem

//noinspection TypeAnnotation
final case class OpenCLScanSeq(n: Nat,
                               initAddrSpace: AddressSpace,
                               dt1: DataType,
                               dt2: DataType,
                               f: Phrase[ExpType ->: ExpType ->: ExpType],
                               init: Phrase[ExpType],
                               array: Phrase[ExpType])
  extends ExpPrimitive {

  f :: expT(dt1, read) ->: expT(dt2, read) ->: expT(dt2, write)
  init :: expT(dt2, write)
  array :: expT(n `.` dt1, read)
  override val t: ExpType = expT(n `.` dt2, write)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    OpenCLScanSeq(fun.nat(n), fun.addressSpace(initAddrSpace), fun.data(dt1), fun.data(dt2),
      VisitAndRebuild(f, fun), VisitAndRebuild(init, fun),
      VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)}) " +
      s"(${PrettyPhrasePrinter(init)}) (${PrettyPhrasePrinter(array)})"

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(expT(n `.` dt1, read))(x =>
      OpenCLScanSeqI(n, initAddrSpace, dt1, dt2,
        λ(expT(dt1, read))(x =>
          λ(expT(dt2, read))(y =>
            λ(accT(dt2))(o =>
              acc(f(x)(y))(o)))),
        init, x, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    //    import TranslationToImperative._

    //TODO think about this more, this allocates memory implicitly
    ???
    //    `new`(n`.`dt2, λ(varT(n`.`dt2))(tmp =>
    //      acc(this)(tmp.wr) `;` C(tmp.rd) ))
  }

  override def xmlPrinter: Elem =
    <reduce n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f type={ToString(
        ExpType(dt1, read) ->: ExpType(dt2, read) ->: ExpType(dt2, read))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <init type={ToString(ExpType(dt2, read))}>
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

//noinspection TypeAnnotation
final case class OpenCLScanSeqInclusive(n: Nat,
                               initAddrSpace: AddressSpace,
                               dt1: DataType,
                               dt2: DataType,
                               f: Phrase[ExpType ->: ExpType ->: ExpType],
                               init: Phrase[ExpType],
                               array: Phrase[ExpType])
  extends ExpPrimitive {

  f :: expT(dt1, read) ->: expT(dt2, read) ->: expT(dt2, write)
  init :: expT(dt2, write)
  array :: expT(n `.` dt1, read)
  override val t: ExpType = expT((n+1) `.` dt2, write)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    OpenCLScanSeqInclusive(fun.nat(n), fun.addressSpace(initAddrSpace), fun.data(dt1), fun.data(dt2),
      VisitAndRebuild(f, fun), VisitAndRebuild(init, fun),
      VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)}) " +
      s"(${PrettyPhrasePrinter(init)}) (${PrettyPhrasePrinter(array)})"

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(expT(n `.` dt1, read))(x =>
      OpenCLScanSeqInclusiveI(n, initAddrSpace, dt1, dt2,
        λ(expT(dt1, read))(x =>
          λ(expT(dt2, read))(y =>
            λ(accT(dt2))(o =>
              acc(f(x)(y))(o)))),
        init, x, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    //    import TranslationToImperative._

    //TODO think about this more, this allocates memory implicitly
    ???
    //    `new`(n`.`dt2, λ(varT(n`.`dt2))(tmp =>
    //      acc(this)(tmp.wr) `;` C(tmp.rd) ))
  }

  override def xmlPrinter: Elem =
    <reduce n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f type={ToString(
        ExpType(dt1, read) ->: ExpType(dt2, read) ->: ExpType(dt2, read))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <init type={ToString(ExpType(dt2, read))}>
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