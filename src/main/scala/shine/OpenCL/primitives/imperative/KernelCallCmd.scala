package shine.OpenCL.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.OpenCL.{GlobalSize, LocalSize}
import shine.macros.Primitive.comPrimitive

@comPrimitive
case class KernelCallCmd(name: String,
                         localSize: LocalSize,
                         globalSize: GlobalSize,
                         output: Phrase[AccType],
                         args: Seq[Phrase[ExpType]]) extends CommandPrimitive

