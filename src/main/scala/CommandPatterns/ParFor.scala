package CommandPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType._
import Compiling.SubstituteImplementations
import apart.arithmetic.{RangeAdd, NamedVar}
import opencl.generator.OpenCLAST
import opencl.generator.OpenCLAST._
import DSL._

abstract class AbstractParFor(val n: Phrase[ExpType],
                              val out: Phrase[AccType],
                              val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends CommandPattern {

  protected var ocl: ToOpenCL = null

  override def typeCheck(): CommandType = {
    import TypeChecker._

    check(TypeChecker(n), ExpType(int))
    val nInt = OperationalSemantics.evalIndexExp(new OperationalSemantics.Store(), n)

    TypeChecker(out) match {
      case AccType(ArrayType(m, dt)) =>
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

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[CommandType] = {
    makeParFor(VisitAndRebuild(n, f), VisitAndRebuild(out, f), VisitAndRebuild(body, f))
  }

  override def substituteImpl: Phrase[CommandType] =
    makeParFor(n, out, SubstituteImplementations.applyBinaryFun(body))

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} ${evalIndexExp(new OperationalSemantics.Store(), n)} ${PrettyPrinter(out)} ${PrettyPrinter(body)}"

  def makeParFor: (Phrase[ExpType], Phrase[AccType], Phrase[ExpType -> (AccType -> CommandType)]) => AbstractParFor

  protected val name: String = newName()

  protected lazy val upperBound = ToOpenCL.exp(n, ocl) match {
    case ArithExpression(ae) => ae
    case _ => throw new Exception("This should not happen")
  }

  def init: OpenCLAST.Declaration
  def cond: OpenCLAST.ExpressionStatement
  def increment: OpenCLAST.Expression
  def synchronize: OpenCLAST.OclAstNode with BlockMember

  override def toOpenCL(block: Block, ocl: ToOpenCL): Block = {
    import opencl.generator.OpenCLAST._

    this.ocl = ocl

    ocl.env(name) = RangeAdd(0, upperBound, 1)

    val i = identifier(name, ExpType(int))
    val body_ = Lift.liftFunction( Lift.liftFunction(body)(i) )
    val out_at_i = out `@` i
    TypeChecker(out_at_i)

    (block: Block) +=
      ForLoop(init, cond, increment, ToOpenCL.cmd(body_(out_at_i), Block(), ocl))

    ocl.env.remove(name)

    (block: Block) += synchronize
  }

}

case class ParFor(override val n: Phrase[ExpType],
                  override val out: Phrase[AccType],
                  override val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends AbstractParFor(n, out, body) {

  override def makeParFor = ParFor

  override lazy val init: Declaration =
    VarDecl(name, opencl.ir.Int,
      init = ArithExpression(0),
      addressSpace = opencl.ir.PrivateMemory)

  override lazy val cond: ExpressionStatement =
    CondExpression(VarRef(name),
      ArithExpression(upperBound),
      CondExpression.Operator.<)

  override lazy val increment: Expression = {
    val v = NamedVar(name)
    AssignmentExpression(ArithExpression(v), ArithExpression(v + 1))
  }

  override def synchronize: OclAstNode with BlockMember = Comment("")
}
