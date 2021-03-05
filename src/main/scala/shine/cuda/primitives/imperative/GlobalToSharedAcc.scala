package shine.cuda.primitives.imperative

import shine.DPIA.DSL.{`new` => _}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

/**
  * Copy a element from global memory to shared memory without using registers
  * (faster than normal copy operations using the =-operator). <br>
  * This requires CUDA 11 and compute capability >= 8. For devices with ompute capability
  * smaller than 8 this will be compiled by the CUDA-Compiler to the same as normal copy
  * operations using the =-operator).
  * @param dt           datatype of element which should be copied
  * @param pipe         pipeline which should be used to execute this copy instruction
  * @param outputShared output-Acceptor in shared memory of type `dt`
  */
@accPrimitive
final case class GlobalToSharedAcc(dt: DataType,
                                   pipe: Phrase[ExpType],
                                   outputShared: Phrase[AccType]
                                  ) extends AccPrimitive {
  pipe :: expT(pipeline, read)
  outputShared :: accT(dt)
  override val t: AccType = accT(dt)

  override def prettyPrint: String =
    s"(GlobalToSharedAcc $pipe, ${PrettyPhrasePrinter(outputShared)})"
}
