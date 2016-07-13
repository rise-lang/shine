package OpenCL.Core

import Core._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.{Block, Expression, VarRef}

trait ViewExp {
  def toOpenCL(env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr], dt: DataType): Expression
}

trait ViewAcc {
  def toOpenCL(env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr], dt: DataType): VarRef
}

trait GeneratableComm {
  def toOpenCL(block: Block, env: ToOpenCL.Environment): Block
}
