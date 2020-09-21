package shine.OpenCL.FunctionalPrimitives

import shine.DPIA.Compilation._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._
import shine.OpenCL.IntermediatePrimitives.OpenCLReduceByIndexSeqI

import scala.xml.Elem

final case class OpenCLReduceByIndexSeq(n: Nat,
                                        k: Nat,
                                        histAddrSpace: shine.DPIA.Types.AddressSpace,
                                        dt: DataType,
                                        f: Phrase[ExpType ->: ExpType ->: ExpType],
                                        hist: Phrase[ExpType],
                                        input: Phrase[ExpType]
                                       ) extends ExpPrimitive {

  f :: expT(dt, read) ->: expT(dt, read) ->: expT(dt, write)
  hist :: expT(k`.`dt, write)
  input :: expT(n`.`PairType(IndexType(k), dt), read)
  override val t: ExpType = expT(k`.`dt, read)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    OpenCLReduceByIndexSeq(fun.nat(n), fun.nat(k), fun.addressSpace(histAddrSpace), fun.data(dt),
      VisitAndRebuild(f, fun), VisitAndRebuild(hist, fun), VisitAndRebuild(input, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${histAddrSpace})" +
      s"(${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(hist)}) (${PrettyPhrasePrinter(input)})"

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._

    con(input)(λ(expT(n`.`PairType(IndexType(k), dt), read))(X =>
      OpenCLReduceByIndexSeqI(n, k, histAddrSpace, dt,
        λ(expT(dt, read))(x =>
          λ(expT(dt, read))(y =>
            λ(accT(dt))(o => acc( f(x)(y) )( o )))),
        hist, X, C)(context)))
  }

  override def xmlPrinter: Elem =
    <reduce n={ToString(n)} addrSpace={ToString(histAddrSpace)}
            k={ToString(n)} dt={ToString(dt)}>
      <f type={ToString(
        ExpType(dt, read) ->: (ExpType(dt, read) ->: ExpType(dt, write)))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <hist type={ToString(ExpType(ArrayType(k, dt), write))}>
        {Phrases.xmlPrinter(hist)}
      </hist>
      <input type={ToString(ExpType(ArrayType(n, PairType(IndexType(k), dt)), read))}>
        {Phrases.xmlPrinter(input)}
      </input>
    </reduce>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })

}
