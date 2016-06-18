package CommandPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType._
import Compiling.SubstituteImplementations
import apart.arithmetic.{RangeAdd, NamedVar}
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

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[CommandType] = {
    For(VisitAndRebuild(n, f), VisitAndRebuild(body, f))
  }

  override def substituteImpl: Phrase[CommandType] =
    For(n, SubstituteImplementations.applyFun(body))

  override def prettyPrint: String = {
    val length = OperationalSemantics.evalIndexExp(new OperationalSemantics.Store(), n)
    s"for 0..$length ${PrettyPrinter(body)}"
  }

  override def toOpenCL(block: Block, ocl: ToOpenCL): Block = {
    import opencl.generator.OpenCLAST._

    val upperBound = ToOpenCL.exp(n, ocl) match {
      case ArithExpression(ae) => ae
      case _ => throw new Exception("This should not happen")
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
}
