package shine.OpenCL.FunctionalPrimitives

import shine.DPIA.Compilation._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._
import shine.OpenCL.IntermediatePrimitives.OpenCLSegReduceAtomicWrgI

import scala.xml.Elem

final case class OpenCLSegReduceAtomicWrg(n: Nat,
                                          k: Nat,
                                          chunkSize: Nat,
                                          initAddrSpace: shine.DPIA.Types.AddressSpace,
                                          dt: DataType,
                                          f: Phrase[ExpType ->: ExpType ->: ExpType],
                                          init: Phrase[ExpType],
                                          array: Phrase[ExpType])
  extends ExpPrimitive {

  f :: expT(dt, read) ->: expT(dt, read) ->: expT(dt, write)
  init :: expT(k`.`dt, write)
  array :: expT(n`.`PairType(IndexType(k), dt), read)
  override val t: ExpType = expT(k`.`dt, read)

  override def visitAndRebuild(
                                fun: VisitAndRebuild.Visitor
                              ): Phrase[ExpType] = {
    OpenCLSegReduceAtomicWrg(fun.nat(n), fun.nat(k), fun.nat(chunkSize), fun.addressSpace(initAddrSpace), fun.data(dt),
      VisitAndRebuild(f, fun), VisitAndRebuild(init, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${initAddrSpace})" +
      s"(${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(init)}) (${PrettyPhrasePrinter(array)})"

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(expT(n`.`PairType(IndexType(k), dt), read))(X =>
      OpenCLSegReduceAtomicWrgI(n, k, chunkSize, initAddrSpace, dt,
        λ(expT(dt, read))(x =>
          λ(expT(dt, read))(y =>
            λ(accT(dt))(o => acc( f(x)(y) )( o )))),
        init, X, C)(context)))
  }

  override def xmlPrinter: Elem =
    <reduce n={ToString(n)} k={ToString(k)} m={ToString(chunkSize)}
            addrSpace={ToString(initAddrSpace)} dt={ToString(dt)}>
      <f type={ToString(
        ExpType(dt, read) ->: (ExpType(dt, read) ->: ExpType(dt, write)))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <init type={ToString(ExpType(ArrayType(k, dt), read))}>
        {Phrases.xmlPrinter(init)}
      </init>
      <array type={ToString(ExpType(ArrayType(n, PairType(IndexType(k), dt)), read))}>
        {Phrases.xmlPrinter(array)}
      </array>
    </reduce>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })

}
