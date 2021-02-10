package shine.OpenCL.primitives.functional

import shine.DPIA._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.OpenCL.{GlobalSize, LocalSize}
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Run(localSize: LocalSize,
                     globalSize: GlobalSize,
                     dt: DataType,
                     input: Phrase[ExpType]
                    ) extends ExpPrimitive {
  input :: expT(dt, write)
  override val t: ExpType = expT(dt, write)
}
