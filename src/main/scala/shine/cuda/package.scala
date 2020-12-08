package shine

import arithexpr.arithmetic.{ArithExpr, ContinuousRange, PosInf, Range, SimplifiedExpr}
import shine.OpenCL.BuiltInFunctionCall

package object cuda {

  val AddressSpace: shine.DPIA.Types.AddressSpace.type =
    shine.DPIA.Types.AddressSpace
  type AddressSpace = shine.DPIA.Types.AddressSpace
  val warpSize = 32

  object gridDim {
    def apply(param: Char, range : Range = ContinuousRange(1, PosInf)) =
      BuiltInAttribute("gridDim", param, range)
  }

  object blockDim {
    def apply(param: Char, range : Range = ContinuousRange(1, PosInf)) =
      BuiltInAttribute("blockDim", param, range)
  }

  object threadId {
    def apply(param: Char) =
      BuiltInAttribute("threadIdx", param, ContinuousRange(0, blockDim(param)))
  }

  object blockId {
    def apply(param: Char) =
      BuiltInAttribute("blockIdx", param, ContinuousRange(0, gridDim(param)))
  }

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
