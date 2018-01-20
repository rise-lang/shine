package idealised.OpenMP

import idealised.DPIA.Nat
import idealised.DPIA.Types.DataType
import idealised.OpenMP.CodeGeneration.CodeGenerator
import opencl.generator.OpenCLAST.{Block, Expression}

import scala.collection.immutable.List

trait ViewExp {
  def toOpenMP(env: CodeGenerator.Environment,
               arrayAccess: List[Nat],
               tupleAccess: List[Nat],
               dt: DataType): Expression
}

trait ViewAcc {
  def toOpenMP(env: CodeGenerator.Environment,
               value: Expression,
               dt: DataType,
               arrayAccess: List[Nat],
               tupleAccess: List[Nat]):  Expression
}

trait GeneratableComm {
  def codeGenCmd(block: Block, env: CodeGenerator.Environment): Block
}

trait GeneratableExp {
  def codeGenExp(env: CodeGenerator.Environment): Expression
}

