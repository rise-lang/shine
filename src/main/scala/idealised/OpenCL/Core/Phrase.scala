package idealised.OpenCL.Core

import idealised.Core._
import opencl.generator.OpenCLAST.{Block, Expression, VarRef}

trait ViewExp {
  def toOpenCL(env: ToOpenCL.Environment,
               arrayAccess: List[(Nat, Nat)],
               tupleAccess: List[Nat], dt: DataType): Expression
}

trait ViewAcc {
  def toOpenCL(env: ToOpenCL.Environment,
               arrayAccess: List[(Nat, Nat)],
               tupleAccess: List[Nat], dt: DataType): VarRef
}

trait GeneratableComm {
  def toOpenCL(block: Block, env: ToOpenCL.Environment): Block
}
