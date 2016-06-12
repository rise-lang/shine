package CommandPatterns

import Core.OperationalSemantics.newName
import Core.PhraseType._
import Core._
import apart.arithmetic.NamedVar
import opencl.generator.OpenCLAST._
import opencl.generator.{get_local_id, get_local_size}


case class ParForLocal(n: Phrase[ExpType],
                       out: Phrase[AccType],
                       body: Phrase[ExpType -> (AccType -> CommandType)])
  extends AbstractParFor(n, out, body) {

  override def makeParFor = ParForLocal

  override def name: NamedVar =
    NamedVar(newName())

  override def init: Declaration =
    VarDecl(name.name, opencl.ir.Int,
      init = ArithExpression(get_local_id(0)),
      addressSpace = opencl.ir.PrivateMemory)

  override def cond: ExpressionStatement =
    CondExpression(VarRef(name.name),
      ToOpenCL.exp(n),
      CondExpression.Operator.<)

  override def increment: Expression =
    AssignmentExpression(ArithExpression(name),
      ArithExpression(name + get_local_size(0)))

}
