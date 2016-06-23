package CommandPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType._
import Compiling.SubstituteImplementations
import apart.arithmetic.{ArithExpr, NamedVar, RangeAdd}
import opencl.generator.OpenCLAST.Block
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

    val in = identifier(inptr, ExpType(ArrayType(n, dt)))
    val out = identifier(outptr, AccType(ArrayType(n, dt)))

    val bodyE = Lift.liftNatDependentFunction(body)
    val bodyEE = Lift.liftFunction(bodyE(loopVar))
    val bodyEEE = Lift.liftFunction(bodyEE(out))

    val nestedBlock = Block()
    val body_ = ToOpenCL.cmd(bodyEEE(in), nestedBlock, ocl)

    nestedBlock += Comment(s"swap($inptr, $outptr);")

    (block: Block) += ForLoop(init, cond, increment, body_)

    // copy result to output
    val CE = Lift.liftFunction(C)

    val tmp = CE(in)
    TypeChecker(tmp)

    (block: Block) += ToOpenCL.cmd(tmp, Block(), ocl)

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

  override def substituteImpl: Phrase[CommandType] = {
    DoubleBufferFor(n, dt,
      buffer1,
      buffer2,
      k,
      SubstituteImplementations.applyNatDependentBinaryFun(body),
      SubstituteImplementations.applyFun(C))
  }

  override def prettyPrint: String = {
    s"doubleBufferFor $buffer1 $buffer2 $k $body $C"
  }

}
