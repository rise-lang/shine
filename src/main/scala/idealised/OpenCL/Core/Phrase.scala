package idealised.OpenCL.Core

import idealised.Core._
import opencl.generator.OpenCLAST.{Block, Expression}

import scala.collection.immutable.List

trait ViewExp {
  def toOpenCL(env: ToOpenCL.Environment,
               arrayAccess: List[(Nat, Nat)],
               tupleAccess: List[Nat], dt: DataType): Expression
}

trait ViewAcc {
  def toOpenCL(env: ToOpenCL.Environment,
               value: Expression,
               dt: DataType,
               arrayAccess: List[(Nat, Nat)],
               tupleAccess: List[Nat]):  Expression
}

trait GeneratableComm {
  def toOpenCL(block: Block, env: ToOpenCL.Environment): Block
}
