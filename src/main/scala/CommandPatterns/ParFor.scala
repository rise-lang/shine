package CommandPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType._
import Rewriting.SubstituteImplementations
import apart.arithmetic.NamedVar
import opencl.generator.OpenCLAST
import opencl.generator.OpenCLAST._
import DSL._

abstract class AbstractParFor(n: Phrase[ExpType],
                              out: Phrase[AccType],
                              body: Phrase[ExpType -> (AccType -> CommandType)])
  extends CommandPattern {

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
        } else error("$nInt != $m", expected = "them to match")
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

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): CommandPattern =
    makeParFor(
      OperationalSemantics.substitute(phrase, `for`, n),
      OperationalSemantics.substitute(phrase, `for`, out),
      OperationalSemantics.substitute(phrase, `for`, body))

  override def substituteImpl: Phrase[CommandType] =
    makeParFor(n, out, SubstituteImplementations.applyBinaryFun(body))

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} ${evalIndexExp(new OperationalSemantics.Store(), n)} ${PrettyPrinter(out)} ${PrettyPrinter(body)}"

  def makeParFor: (Phrase[ExpType], Phrase[AccType], Phrase[ExpType -> (AccType -> CommandType)]) => AbstractParFor

  def name: NamedVar
  def init: OpenCLAST.Declaration
  def cond: OpenCLAST.ExpressionStatement
  def increment: OpenCLAST.Expression

  override def toOpenCL(block: Block): Block = {
    import opencl.generator.OpenCLAST._

    val i = identifier(name.name, ExpType(int))
    val body_ = Lift.liftFunction( Lift.liftFunction(body)(i) )
    val out_at_i = out `@` i
    TypeChecker(out_at_i)

    (block: Block) +=
      ForLoop(init, cond, increment, ToOpenCL.cmd(body_(out_at_i), Block()))
  }

}

case class ParFor(n: Phrase[ExpType],
                  out: Phrase[AccType],
                  body: Phrase[ExpType -> (AccType -> CommandType)])
  extends AbstractParFor(n, out, body) {

  override def makeParFor = ParFor

  override val name: NamedVar =
    NamedVar(newName())

  override val init: Declaration =
    VarDecl(name.name, opencl.ir.Int,
      init = ArithExpression(0),
      addressSpace = opencl.ir.PrivateMemory)

  override val cond: ExpressionStatement =
    CondExpression(VarRef(name.name),
      ToOpenCL.exp(n),
      CondExpression.Operator.<)

  override val increment: Expression =
    AssignmentExpression(ArithExpression(name),
      ArithExpression(name + 1))

}
