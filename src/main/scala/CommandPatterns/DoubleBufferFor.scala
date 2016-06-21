package CommandPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType._
import Compiling.SubstituteImplementations
import Core.VisitAndRebuild.fun
import apart.arithmetic.{NamedVar, RangeAdd}
import opencl.generator.OpenCLAST.Block
import DSL._

case class DoubleBufferFor(n: Phrase[ExpType],
                           buffer1: Phrase[VarType],
                           buffer2: Phrase[VarType],
                           body: Phrase[ExpType -> CommandType])
  extends CommandPattern {

  override def toOpenCL(block: Block, ocl: ToOpenCL): Block = {
    import opencl.generator.OpenCLAST._

    val upperBound = ToOpenCL.exp(n, ocl) match {
      case ArithExpression(ae) => ae
      case _ => throw new Exception
    }

    val name = newName()

    ocl.env(name) = RangeAdd(0, upperBound, 1)

    val init = VarDecl(name, opencl.ir.Int,
      init = ArithExpression(0),
      addressSpace = opencl.ir.PrivateMemory)

    val cond = CondExpression(VarRef(name),
      ArithExpression(upperBound),
      CondExpression.Operator.<)

    val v = NamedVar(name)
    val increment = AssignmentExpression(ArithExpression(v), ArithExpression(v + 1))

    val bodyE = Lift.liftFunction(body)
    val i = identifier(name, ExpType(int))

    val body_ = ToOpenCL.cmd(bodyE(i), Block(), ocl)

    (block: Block) += ForLoop(init, cond, increment, body_)

    ocl.env.remove(name)

    block
  }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(f: fun): Phrase[CommandType] = ???

  override def typeCheck(): CommandType = ???

  override def substituteImpl: Phrase[CommandType] = ???

  override def prettyPrint: String = ???

}
