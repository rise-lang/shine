package CommandPatterns

import Core.OperationalSemantics.newName
import Core.PhraseType._
import Core._
import apart.arithmetic.NamedVar
import opencl.generator.OpenCLAST._
import opencl.generator.{get_global_id, get_global_size}


case class ParForGlobal(n: Phrase[ExpType],
                           out: Phrase[AccType],
                           body: Phrase[ExpType -> (AccType -> CommandType)])
  extends AbstractParFor(n, out, body) {

  override val makeParFor = ParForGlobal

  override val name: NamedVar =
    NamedVar(newName())

  override val init: Declaration =
    VarDecl(name.name, opencl.ir.Int,
      init = ArithExpression(get_global_id(0)),
      addressSpace = opencl.ir.PrivateMemory)

  override val cond: ExpressionStatement =
    CondExpression(VarRef(name.name),
      ToOpenCL.exp(n),
      CondExpression.Operator.<)

  override val increment: Expression =
    AssignmentExpression(ArithExpression(name),
      ArithExpression(name + get_global_size(0)))

}
