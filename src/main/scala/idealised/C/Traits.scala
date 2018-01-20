package idealised.C

import idealised.C.AST.{Block, Expr}
import idealised.DPIA.Nat
import idealised.DPIA.Types.DataType
import idealised.C.CodeGeneration.CodeGenerator

import scala.collection.immutable.List

trait ViewExp {
  def toOpenMP(env: CodeGenerator.Environment,
               arrayAccess: List[Nat],
               tupleAccess: List[Nat],
               dt: DataType): Expr
}

trait ViewAcc {
  def toOpenMP(env: CodeGenerator.Environment,
               value: Expr,
               dt: DataType,
               arrayAccess: List[Nat],
               tupleAccess: List[Nat]):  Expr
}

trait GeneratableComm {
  def codeGenCmd(block: Block, env: CodeGenerator.Environment): Block
}

trait GeneratableExp {
  def codeGenExp(env: CodeGenerator.Environment): Expr
}

