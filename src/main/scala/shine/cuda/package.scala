package shine

import arithexpr.arithmetic.{ArithExpr, ContinuousRange, PosInf, Range, SimplifiedExpr}
import shine.OpenCL.BuiltInFunctionCall

package object cuda {

  val AddressSpace: shine.DPIA.Types.AddressSpace.type =
    shine.DPIA.Types.AddressSpace
  type AddressSpace = shine.DPIA.Types.AddressSpace
  val warpSize = 32

  def gridDim: OpenCL.get_num_groups.type = shine.OpenCL.get_num_groups
  def blockDim: OpenCL.get_local_size.type = shine.OpenCL.get_local_size
  def threadId: OpenCL.get_local_id.type = shine.OpenCL.get_local_id
  def blockId: OpenCL.get_group_id.type = shine.OpenCL.get_group_id

  object warpId {
    def apply(param: Char): ArithExpr with SimplifiedExpr =
      threadId(param) / warpSize
  }

  object warpDim {
    def apply(param: Char): ArithExpr with SimplifiedExpr =
      blockDim(param) / warpSize
  }

  object laneId {
    def apply(param: Char): ArithExpr with SimplifiedExpr =
      threadId(param) % warpSize
  }

  object globalId {
    def apply(param: Char): ArithExpr with SimplifiedExpr =
      blockId(param) * blockDim(param) + threadId(param)
  }

  object globalDim {
    def apply(param: Char): ArithExpr with SimplifiedExpr =
      blockDim(param) * gridDim(param)
  }
}
