package idealised.OpenCL

import idealised.DPIA.Nat
import idealised.DPIA.Types.DataType
import idealised.OpenCL
import idealised.OpenCL.CodeGeneration.OpenCLOldCodeGenerator
import opencl.generator.OpenCLAST.{Block, Expression}
import OpenCL.CodeGeneration.{CodeGenerator => OpenCLCodeGenerator}

import scala.collection.immutable.List

trait ViewExp {
  def toOpenCL(env: OpenCLOldCodeGenerator.Environment,
               arrayAccess: List[Nat],
               tupleAccess: List[Nat],
               dt: DataType): Expression
}

trait ViewAcc {
  def toOpenCL(env: OpenCLOldCodeGenerator.Environment,
               value: Expression,
               dt: DataType,
               arrayAccess: List[Nat],
               tupleAccess: List[Nat]):  Expression
}

trait GeneratableComm {
  def codeGenCmd(block: Block, env: OpenCLOldCodeGenerator.Environment): Block
}

trait GeneratableExp {
  def codeGenExp(env: OpenCLOldCodeGenerator.Environment): Expression
}


trait GeneratableOpenCLCommand {
  def codeGen(gen: OpenCLCodeGenerator)(env: OpenCLCodeGenerator.Environment): OpenCLCodeGenerator.Stmt
}

trait GeneratableOpenCLExp {
  def codeGen(gen: OpenCLCodeGenerator)(env: OpenCLCodeGenerator.Environment, path: OpenCLCodeGenerator.Path): OpenCLCodeGenerator.Expr
}

trait GeneratableOpenCLAcc {
  def codeGen(gen: OpenCLCodeGenerator)(env: OpenCLCodeGenerator.Environment, path: OpenCLCodeGenerator.Path): OpenCLCodeGenerator.Expr
}
