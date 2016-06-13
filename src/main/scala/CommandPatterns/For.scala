package CommandPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType._
import Rewriting.SubstituteImplementations
import apart.arithmetic.NamedVar
import opencl.generator.OpenCLAST.Block
import DSL._

case class For(n: Phrase[ExpType],
               body: Phrase[ExpType -> CommandType])
  extends CommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    check(TypeChecker(n), ExpType(int))
    check(TypeChecker(body), FunctionType(ExpType(int), CommandType()))
    CommandType()
  }

  override def eval(s: Store): Store = {
    val nE = evalIndexExp(s, n)
    val bodyE = OperationalSemantics.eval(s, body)
    (0 until nE.eval).foldLeft(s)( (s1, i) => {
      OperationalSemantics.eval(s1, bodyE(LiteralPhrase(i)))
    } )
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]) =
    For(
      OperationalSemantics.substitute(phrase, `for`, n),
      OperationalSemantics.substitute(phrase, `for`, body))

  override def substituteImpl: Phrase[CommandType] =
    For(n, SubstituteImplementations.applyFun(body))

  override def prettyPrint: String = {
    val length = OperationalSemantics.evalIndexExp(new OperationalSemantics.Store(), n)
    s"for 0..$length ${PrettyPrinter(body)}"
  }

  override def toOpenCL(block: Block): Block = {
    import opencl.generator.OpenCLAST._

    val name = NamedVar(newName())
    val init = VarDecl(name.name, opencl.ir.Int,
      init = ArithExpression(0),
      addressSpace = opencl.ir.PrivateMemory)

    val cond = CondExpression(VarRef(name.name),
      ToOpenCL.exp(n),
      CondExpression.Operator.<)

    val increment = AssignmentExpression(ArithExpression(name), ArithExpression(name + 1))

    val bodyE = Lift.liftFunction(body)
    val i = identifier(name.name, ExpType(int))

    val body_ = ToOpenCL.cmd(bodyE(i), Block())

    (block: Block) += ForLoop(init, cond, increment, body_)
  }
}
