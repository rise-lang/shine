package idealised.OpenMP.ImperativePrimitives


import idealised.DPIA.DSL.{identifier, _}
import idealised.DPIA.ImperativePrimitives.AbstractParFor
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenMP.CodeGeneration.CodeGenerator
import idealised.OpenMP.GeneratableComm
import lift.arithmetic.{Cst, NamedVar, RangeAdd}
import opencl.generator.OpenCLAST
import opencl.generator.OpenCLAST.Block

case class OpenMPParFor(override val n: Nat,
                        override val dt: DataType,
                        override val out: Phrase[AccType],
                        override val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends AbstractParFor(n, dt, out, body) with GeneratableComm {

  override def makeParFor:
    (Nat, DataType, Phrase[AccType], Phrase[->[ExpType, ->[AccType, CommandType]]]) => OpenMPParFor = OpenMPParFor

  val name: String = freshName("i_")
  val init: Nat = Cst(0)
  val step: Nat = Cst(1)

  override def codeGenCmd(block: Block, env: CodeGenerator.Environment): Block = {
    import opencl.generator.OpenCLAST._

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

    val bodyBlock = (b: Block) => CodeGenerator.cmd(body_(out_at_i), b, updatedEnv)

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
          (block: Block) += OpenCLAST.OpenCLCode("#pragma omp parallel for")
          (block: Block) +=
            ForLoop(initDecl, cond, increment, bodyBlock(Block()))
        }

    }

    (block: Block) += OpenCLAST.Skip()
  }
}
