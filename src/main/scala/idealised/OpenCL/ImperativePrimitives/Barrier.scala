package idealised.OpenCL.ImperativePrimitives

import idealised.DPIA.Phrases.{CommandPrimitive, Phrase, PrettyPhrasePrinter, ToString, VisitAndRebuild}
import idealised.DPIA.Semantics.OperationalSemantics.Store
import idealised.DPIA.Types.{CommType, comm}

import scala.xml.Elem

final case class Barrier(local: Boolean, global: Boolean) extends CommandPrimitive {
  override val t: CommType = comm

  override def prettyPrint: String =
    s"""barrier( ${if(local) "CLK_LOCAL_MEM_FENCE" else ""} ${if(global && local) "|" else ""}
      ${if(global) "CLK_GLOBAL_MEM_FENCE" else ""})"""

  override def xmlPrinter: Elem = <barrier local={ToString(local)} global={ToString(global)} />

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommType] = Barrier(local, global)

  override def eval(s: Store): Store = ???
}
