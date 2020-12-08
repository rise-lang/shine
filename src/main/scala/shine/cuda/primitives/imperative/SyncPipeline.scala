package shine.cuda.primitives.imperative

import shine.DPIA.Phrases.{CommandPrimitive, Phrase, ToString, VisitAndRebuild}
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types.{CommType, ExpType, pipeline, read}
import shine.DPIA.expT

import scala.xml.Elem

final case class SyncPipeline(pipe: Phrase[ExpType]
                             ) extends CommandPrimitive {

  pipe :: expT(pipeline, read)

  override def prettyPrint: String = s"$pipe.commit_and_wait()"

  override def xmlPrinter: Elem = <synchronizePipeline pipe={ToString(pipe)}/>

  override def visitAndRebuild(f: VisitAndRebuild.Visitor
                              ): Phrase[CommType] = SyncPipeline(VisitAndRebuild(pipe, f))

  override def eval(s: Store): Store = ???
}
