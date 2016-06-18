package CommandPatterns

import Core.OperationalSemantics.newName
import Core.PhraseType._
import Core._
import apart.arithmetic._
import opencl.generator.OpenCLAST._
import opencl.generator.{get_group_id, get_num_groups}


case class ParForWorkgroup(override val n: Phrase[ExpType],
                           override val out: Phrase[AccType],
                           override val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends AbstractParFor(n, out, body) {

  lazy val num_groups =
    if (ocl.globalSize == ? || ocl.localSize == ?) ?
    else ocl.globalSize / ocl.localSize

  override def makeParFor = ParForWorkgroup

  override lazy val init: Declaration =
    VarDecl(name, opencl.ir.Int,
      init = ArithExpression(get_group_id(0, RangeAdd(0, num_groups, 1))),
      addressSpace = opencl.ir.PrivateMemory)

  override lazy val cond: ExpressionStatement =
    CondExpression(VarRef(name),
      ArithExpression(upperBound),
      CondExpression.Operator.<)

  lazy val num_groups_range =
    if (num_groups == ?)  ContinuousRange(1, PosInf)
    else RangeAdd(num_groups, num_groups + 1, 1)

  override lazy val increment: Expression = {
    val v = NamedVar(name)
    AssignmentExpression(ArithExpression(v),
      ArithExpression(v + get_num_groups(0, num_groups_range)))
  }

  override def synchronize: OclAstNode with BlockMember = Comment("par for workgroup sync")

}
