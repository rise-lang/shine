package CommandPatterns

import Core.OperationalSemantics.newName
import Core.PhraseType._
import Core._
import apart.arithmetic.NamedVar
import opencl.generator.OpenCLAST._
import opencl.generator.{get_local_id, get_local_size}


case class ParForLocal(override val n: Phrase[ExpType],
                       override val out: Phrase[AccType],
                       override val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends AbstractParFor(n, out, body) {

  override def makeParFor = ParForLocal

  override val name: NamedVar =
    NamedVar(newName())

  override val init: Declaration =
    VarDecl(name.name, opencl.ir.Int,
      init = ArithExpression(get_local_id(0)),
      addressSpace = opencl.ir.PrivateMemory)

  override val cond: ExpressionStatement =
    CondExpression(VarRef(name.name),
      ToOpenCL.exp(n),
      CondExpression.Operator.<)

  override val increment: Expression =
    AssignmentExpression(ArithExpression(name),
      ArithExpression(name + get_local_size(0)))

}
