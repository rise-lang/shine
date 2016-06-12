package CommandPatterns

import Core.OperationalSemantics.newName
import Core.PhraseType._
import Core._
import apart.arithmetic.NamedVar
import opencl.generator.OpenCLAST._
import opencl.generator.{get_num_groups, get_group_id}


case class ParForWorkgroup(n: Phrase[ExpType],
                           out: Phrase[AccType],
                           body: Phrase[ExpType -> (AccType -> CommandType)])
  extends AbstractParFor(n, out, body) {

  override def makeParFor = ParForWorkgroup

  override def name: NamedVar =
    NamedVar(newName())

  override def init: Declaration =
    VarDecl(name.name, opencl.ir.Int,
      init = ArithExpression(get_group_id(0)),
      addressSpace = opencl.ir.PrivateMemory)

  override def cond: ExpressionStatement =
    CondExpression(VarRef(name.name),
      ToOpenCL.exp(n),
      CondExpression.Operator.<)

  override def increment: Expression =
    AssignmentExpression(ArithExpression(name),
      ArithExpression(name + get_num_groups(0)))

}
