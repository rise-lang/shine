package shine.OpenCL.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

final case class AtomicOperation(dt: DataType,
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
    AtomicOperation(fun.data(dt), VisitAndRebuild(f, fun), VisitAndRebuild(dst, fun), VisitAndRebuild(src, fun))
  }

  override def prettyPrint: String = s"atomic_${PrettyPhrasePrinter(f)}" +
    s"(${PrettyPhrasePrinter(dst)}, ${PrettyPhrasePrinter(src)}"

  override def xmlPrinter: Elem =
    <atomic dt={ToString(dt)}>
      <f>
        {Phrases.xmlPrinter(f)}
      </f>
      <dst>
        {Phrases.xmlPrinter(dst)}
      </dst>
      <src>
        {Phrases.xmlPrinter(src)}
      </src>
    </atomic>

  override def toString: String = s"atomic_(${dt.toString}(${dst.toString}, ${src.toString})"
}
