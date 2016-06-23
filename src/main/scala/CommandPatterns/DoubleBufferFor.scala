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
    import opencl.generator.OpenCLAST._

    // in* = buffer1
    val buffer1Name = buffer1 match {
      case i: IdentPhrase[VarType] => i.name
    }
    val inptr = newName()

    (block: Block) += VarDecl(inptr, DataType.toType(dt),
      init = VarRef(buffer1Name), addressSpace = opencl.ir.PrivateMemory)

    // out* = buffer2
    val buffer2Name = buffer2 match {
      case i: IdentPhrase[VarType] => i.name
    }
    val outptr = newName()

    (block: Block) += VarDecl(outptr, DataType.toType(dt),
      init = VarRef(buffer2Name), addressSpace = opencl.ir.PrivateMemory)

    // for ...
    val name = newName()
    val loopVar = NamedVar(name)

    ocl.env(name) = RangeAdd(0, k, 1)

    val init = VarDecl(name, opencl.ir.Int,
      init = ArithExpression(0),
      addressSpace = opencl.ir.PrivateMemory)

    val cond = CondExpression(VarRef(name),
      ArithExpression(k),
      CondExpression.Operator.<)

    val increment = AssignmentExpression(ArithExpression(loopVar), ArithExpression(loopVar + 1))

    val bodyE = Lift.liftNatDependentFunction(body)
    val tmp = bodyE(loopVar)
    TypeChecker(tmp)
    val bodyEE  = Lift.liftFunction(tmp)

    val in = identifier(inptr, ExpType(dt))
    val out = identifier(outptr, AccType(dt))

    val body_ = ToOpenCL.cmd(bodyEE(out)(in), Block(), ocl)

    (block: Block) += ForLoop(init, cond, increment, body_)

    ocl.env.remove(name)

    block
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
