package shine.OpenCL.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.{GlobalSize, LocalSize}
import shine.macros.Primitive.expPrimitive

@expPrimitive
case class KernelCall(name: String,
                      localSize: LocalSize,
                      globalSize: GlobalSize,
                      inTs: Seq[DataType],
                      outT: DataType,
                      args: Seq[Phrase[ExpType]]) extends ExpPrimitive {
  (inTs zip args).foreach{
    case (inT, arg) => arg :: expT(inT, read)
  }
  override val t: ExpType = expT(outT, write)
}
