package shine

import arithexpr.arithmetic.{ArithExpr, SimplifiedExpr}
import rise.core.types.{DataType, Fragment, MatrixLayoutIdentifier}
import shine.DPIA.Nat
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.ExpType

package object cuda {

  val AddressSpace: rise.core.types.AddressSpace.type =
    rise.core.types.AddressSpace
  type AddressSpace = rise.core.types.AddressSpace
  val warpSize = 32

  def gridDim: OpenCL.get_num_groups.type = shine.OpenCL.get_num_groups
  def blockDim: OpenCL.get_local_size.type = shine.OpenCL.get_local_size
  def threadId: OpenCL.get_local_id.type = shine.OpenCL.get_local_id
  def blockId: OpenCL.get_group_id.type = shine.OpenCL.get_group_id

  object warpId {
    def apply(param: Int): ArithExpr with SimplifiedExpr =
      threadId(param) / warpSize
  }

  object warpDim {
    def apply(param: Int): ArithExpr with SimplifiedExpr =
      blockDim(param) / warpSize
  }

  object laneId {
    def apply(param: Int): ArithExpr with SimplifiedExpr =
      threadId(param) % warpSize
  }

  object globalId {
    def apply(param: Int): ArithExpr with SimplifiedExpr =
      blockId(param) * blockDim(param) + threadId(param)
  }

  object globalDim {
    def apply(param: Int): ArithExpr with SimplifiedExpr =
      blockDim(param) * gridDim(param)
  }

  object AsFragment{
    def apply(rows: Nat, columns: Nat, layers: Nat, dataType: DataType,
              frag: Fragment, matrix: Phrase[ExpType]): shine.cuda.primitives.functional.AsFragment =
      shine.cuda.primitives.functional.AsFragment(rows, columns, layers, dataType,
        frag, MatrixLayoutIdentifier("ml"), matrix)
  }
}
