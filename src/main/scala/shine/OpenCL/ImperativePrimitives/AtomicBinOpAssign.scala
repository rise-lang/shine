package shine.OpenCL.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

final case class AtomicBinOpAssign(dt: DataType,
                                   addrSpace: AddressSpace,
                                   f: Phrase[ExpType ->: ExpType ->: AccType ->: CommType],
                                   dst: Phrase[AccType],
                                   src: Phrase[ExpType])
  extends CommandPrimitive {

  f :: expT(dt, read) ->: expT(dt, read) ->: accT(dt) ->: comm
  dst :: accT(dt)
  src :: expT(dt, read)

  override val t: CommType = comm

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    AtomicBinOpAssign(fun.data(dt), fun.addressSpace(addrSpace),
      VisitAndRebuild(f, fun), VisitAndRebuild(dst, fun), VisitAndRebuild(src, fun))
  }

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${addrSpace})" +
    s"(${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(dst)}) (${PrettyPhrasePrinter(src)})"

  override def xmlPrinter: Elem =
    <atomic dt={ToString(dt)} addrSpace={ToString(addrSpace)}>
      <f type={ToString(ExpType(dt, read) ->: ExpType(dt, read) ->: AccType(dt) ->: CommType())}>
        {Phrases.xmlPrinter(f)}
      </f>
      <dst type={ToString(AccType(dt))}>
        {Phrases.xmlPrinter(dst)}
      </dst>
      <src type={ToString(ExpType(dt, read))}>
        {Phrases.xmlPrinter(src)}
      </src>
    </atomic>
}
