package shine.OpenCL.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.OpenCL.AccessFlags
import shine.macros.Primitive.comPrimitive

// In a host program which contains host managed buffers,
// A host execution is a section where only plain arrays are used
@comPrimitive
final case class HostExecution(params: Map[Identifier[_ <: PhraseType], AccessFlags],
                               body: Phrase[CommType]) extends CommandPrimitive {
  body :: comm

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommType] =
    HostExecution(
      params.map({ case (k, v) =>
        VisitAndRebuild(k, f).asInstanceOf[Identifier[_ <: PhraseType]] -> v }),
      VisitAndRebuild(body, f))
}