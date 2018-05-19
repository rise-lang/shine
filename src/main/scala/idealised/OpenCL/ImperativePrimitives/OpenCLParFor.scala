package idealised.OpenCL.ImperativePrimitives

import idealised.DPIA.Compilation.CodeGenerator
import idealised.DPIA.DSL.{identifier, _}
import idealised.DPIA.ImperativePrimitives.AbstractParFor
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.CodeGeneration.OpenCLOldCodeGenerator
import idealised.OpenCL.GeneratableComm
import lift.arithmetic.{Cst, NamedVar, RangeAdd}
import opencl.generator.OpenCLAST
import opencl.generator.OpenCLAST.{Block, BlockMember}

abstract class OpenCLParFor(n: Nat,
                            dt: DataType,
                            out: Phrase[AccType],
                            body: Phrase[ExpType -> (AccType -> CommandType)])
  extends AbstractParFor(n, dt, out, body) with GeneratableComm {

  def parallelismLevel: idealised.OpenCL.ParallelismLevel

  protected var env: OpenCLOldCodeGenerator.Environment = _

  protected def name: String

  def init: Nat
  def step: Nat
  def synchronize: OpenCLAST.OclAstNode with BlockMember

  override def codeGenCmd(block: Block, env: OpenCLOldCodeGenerator.Environment): Block = {
    import opencl.generator.OpenCLAST._

    this.env = env

    val range = RangeAdd(init, n, step)

    val updatedEnv = env.updatedRanges(name, range)

    val i = identifier(name, exp"[idx($n)]")
    val body_ = Lifting.liftFunction( Lifting.liftFunction(body)(i) )
    val out_at_i = out `@` i
    TypeChecker(out_at_i)

    val initDecl = VarDecl(name, opencl.ir.Int,
      init = ArithExpression(init),
      addressSpace = opencl.ir.PrivateMemory)

    val cond = CondExpression(VarRef(name),
      ArithExpression(n),
      CondExpression.Operator.<)


    val increment = {
      val v = NamedVar(name)
      AssignmentExpression(ArithExpression(v), ArithExpression(v + step))
    }

    val bodyBlock = (b: Block) => OpenCLOldCodeGenerator.cmd(body_(out_at_i), b, updatedEnv)

    range.numVals match {
      case Cst(0) =>
        (block: Block) +=
          OpenCLAST.Comment("iteration count is 0, no loop emitted")

      case Cst(1) =>
        (block: Block) +=
          OpenCLAST.Comment("iteration count is exactly 1, no loop emitted")
        (block: Block) += bodyBlock(Block(Vector(initDecl)))

      case _ =>
        if ( (range.start.min.min == Cst(0) && range.stop == Cst(1))
          || (range.numVals.min == Cst(0) && range.numVals.max == Cst(1)) ) {
          (block: Block) +=
            OpenCLAST.Comment("iteration count is 1 or less, no loop emitted")
          val ifthenelse =
            IfThenElse(CondExpression(
              ArithExpression(init),
              ArithExpression(n),
              CondExpression.Operator.<), bodyBlock(Block()))
          (block: Block) += Block(Vector(initDecl, ifthenelse))
        } else {
          (block: Block) +=
            ForLoop(initDecl, cond, increment, bodyBlock(Block()))
        }

    }

    (block: Block) += synchronize
  }

  override def codeGen[Environment, Path, Stmt, Expr, Decl](gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl])(env: Environment): Stmt = ???
}
