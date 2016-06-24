package CommandPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType._
import Compiling.SubstituteImplementations
import apart.arithmetic.{ArithExpr, Cst, NamedVar, RangeAdd}
import opencl.generator.OpenCLAST
import opencl.generator.OpenCLAST._
import DSL._

abstract class AbstractParFor(val n: ArithExpr,
                              val dt: DataType,
                              val out: Phrase[AccType],
                              val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends CommandPattern {

  protected var ocl: ToOpenCL = null

  override def typeCheck(): CommandType = {
    import TypeChecker._

    check(TypeChecker(n), ExpType(int))
    val nInt = OperationalSemantics.evalIndexExp(new OperationalSemantics.Store(), n)

    TypeChecker(out) match {
      case AccType(ArrayType(m, dt_)) if dt_ == dt =>
        if (nInt == m) {
          TypeChecker(body) match {
            case FunctionType(ExpType(i), FunctionType(AccType(dt2), CommandType())) =>
              if (i == int && dt == dt2) {
                CommandType()
              } else error(s"$i, $dt, and $dt2", expected = "to be int and the last two to match")
            case t_ => error(t_.toString, expected = "FunctionType")
          }
        } else error(s"$nInt != $m", expected = "them to match")
      case t_ => error(t_.toString, expected = "ArrayType")
    }
  }

  override def eval(s: Store): Store = {
    val nE = evalIndexExp(s, n)
    val bodyE = OperationalSemantics.eval(s, body)(OperationalSemantics.BinaryFunctionEvaluator)

    (0 until nE.eval).foldLeft(s)((s1, i) => {
      OperationalSemantics.eval(s1, bodyE(LiteralPhrase(i))(out `@` i))
    })
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    makeParFor(fun(n), fun(dt), VisitAndRebuild(out, fun), VisitAndRebuild(body, fun))
  }

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] =
    makeParFor(n, dt, out, SubstituteImplementations.applyBinaryFun(body, env))

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} ${evalIndexExp(new OperationalSemantics.Store(), n)} ${PrettyPrinter(out)} ${PrettyPrinter(body)}"

  def makeParFor: (ArithExpr, DataType, Phrase[AccType], Phrase[ExpType -> (AccType -> CommandType)]) => AbstractParFor

  protected val name: String = newName()

  def init: ArithExpr
  def step: ArithExpr
  def synchronize: OpenCLAST.OclAstNode with BlockMember

  override def toOpenCL(block: Block, ocl: ToOpenCL): Block = {
    import opencl.generator.OpenCLAST._

    this.ocl = ocl

    val range = RangeAdd(init, n, step)

    ocl.env(name) = range

    val i = identifier(name, ExpType(int))
    val body_ = Lift.liftFunction( Lift.liftFunction(body)(i) )
    val out_at_i = out `@` i
    TypeChecker(out_at_i)

    val initDecl = VarDecl(name, opencl.ir.Int,
      init = ArithExpression(init),
      addressSpace = opencl.ir.PrivateMemory)

    val cond = CondExpression(VarRef(name),
      ArithExpression(n),
      CondExpression.Operator.<)


    val increment: Expression = {
      val v = NamedVar(name)
      AssignmentExpression(ArithExpression(v), ArithExpression(v + step))
    }

    val bodyBlock = (b: Block) => ToOpenCL.cmd(body_(out_at_i), b, ocl)

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

    ocl.env.remove(name)

    (block: Block) += synchronize
  }

}

case class ParFor(override val n: ArithExpr,
                  override val dt: DataType,
                  override val out: Phrase[AccType],
                  override val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends AbstractParFor(n, dt, out, body) {

  override def makeParFor = ParFor

  override lazy val init = Cst(0)

  override lazy val step = Cst(1)

  override def synchronize: OclAstNode with BlockMember = OpenCLAST.Skip()
}
