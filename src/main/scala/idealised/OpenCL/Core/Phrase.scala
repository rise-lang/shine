package idealised.OpenCL.Core

import idealised.Core._
import idealised.OpenCL.CodeGenerator
import opencl.generator.OpenCLAST.{Block, Expression}

import scala.collection.immutable.List

trait ViewExp {
  def toOpenCL(env: CodeGenerator.Environment,
               arrayAccess: List[(Nat, Nat)],
               tupleAccess: List[Nat], dt: DataType): Expression
}

trait ViewAcc {
  def toOpenCL(env: CodeGenerator.Environment,
               value: Expression,
               dt: DataType,
               arrayAccess: List[(Nat, Nat)],
               tupleAccess: List[Nat]):  Expression
}

trait GeneratableComm {
  def codeGenCmd(block: Block, env: CodeGenerator.Environment): Block
}

trait GeneratableExp {
  def codeGenExp(env: CodeGenerator.Environment): Expression
}
