package CommandPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType._
import Compiling.SubstituteImplementations
import Core.VisitAndRebuild.fun
import apart.arithmetic.{ArithExpr, NamedVar, RangeAdd}
import opencl.generator.OpenCLAST.{Block, Comment}
import DSL._

case class DoubleBufferFor(n: ArithExpr,
                           dt: DataType,
                           buffer1: Phrase[VarType],
                           buffer2: Phrase[VarType],
                           k: ArithExpr,
                           body: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
                           C: Phrase[ExpType -> CommandType])
  extends CommandPattern {

  override def toOpenCL(block: Block, ocl: ToOpenCL): Block = {
//    import opencl.generator.OpenCLAST._
//
//    val upperBound = ToOpenCL.exp(n, ocl) match {
//      case ArithExpression(ae) => ae
//      case _ => throw new Exception
//    }
//
//    val name = newName()
//
//    ocl.env(name) = RangeAdd(0, upperBound, 1)
//
//    val init = VarDecl(name, opencl.ir.Int,
//      init = ArithExpression(0),
//      addressSpace = opencl.ir.PrivateMemory)
//
//    val cond = CondExpression(VarRef(name),
//      ArithExpression(upperBound),
//      CondExpression.Operator.<)
//
//    val v = NamedVar(name)
//    val increment = AssignmentExpression(ArithExpression(v), ArithExpression(v + 1))
//
//    val bodyE = Lift.liftFunction(body)
//    val i = identifier(name, ExpType(int))
//
//    val body_ = ToOpenCL.cmd(bodyE(i), Block(), ocl)
//
//    (block: Block) += ForLoop(init, cond, increment, body_)
//
//    ocl.env.remove(name)
//
//    block
    (block: Block) += Comment("double buffer for")
  }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    DoubleBufferFor(fun(n), fun(dt),
      VisitAndRebuild(buffer1, fun),
      VisitAndRebuild(buffer2, fun),
      fun(k),
      VisitAndRebuild(body, fun),
      VisitAndRebuild(C, fun))
  }

  override def typeCheck(): CommandType = {
    CommandType()
  }

  override def substituteImpl: Phrase[CommandType] = ???

  override def prettyPrint: String = {
    s"doubleBufferFor $buffer1 $buffer2 $k $body $C"
  }

}
