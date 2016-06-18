package CommandPatterns

import Core.PhraseType._
import Core._
import apart.arithmetic.{NamedVar, RangeAdd}
import opencl.generator.OpenCLAST._
import opencl.generator.{get_global_id, get_global_size}


case class ParForGlobal(override val n: Phrase[ExpType],
                        override val out: Phrase[AccType],
                        override val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends AbstractParFor(n, out, body) {

  override val makeParFor = ParForGlobal

  override lazy val init: Declaration =
    VarDecl(name, opencl.ir.Int,
      init = ArithExpression(get_global_id(0, RangeAdd(0, ocl.globalSize, 1))),
      addressSpace = opencl.ir.PrivateMemory)

  override lazy val cond: ExpressionStatement =
    CondExpression(VarRef(name),
      ArithExpression(upperBound),
      CondExpression.Operator.<)

  override lazy val increment: Expression = {
    val v = NamedVar(name)
    AssignmentExpression(ArithExpression(v),
      ArithExpression(v + get_global_size(0, RangeAdd(ocl.globalSize, ocl.globalSize + 1, 1))))
  }

  override def synchronize: OclAstNode with BlockMember = Comment("par for global sync")
}
