package CommandPatterns

import Core._
import Core.OperationalSemantics._
import Rewriting.SubstituteImplementations
import opencl.generator.OpenCLAST.Block

case class Seq(c1: Phrase[CommandType], c2: Phrase[CommandType]) extends CommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    check(TypeChecker(c1), CommandType())
    check(TypeChecker(c2), CommandType())
    CommandType()
  }

  override def eval(s: Store): Store = {
    val s1 = OperationalSemantics.eval(s, c1)
    OperationalSemantics.eval(s1, c2)
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[CommandType] = {
    Seq(VisitAndRebuild(c1, f), VisitAndRebuild(c2, f))
  }

  override def substituteImpl: Phrase[CommandType] = Seq(SubstituteImplementations(c1), SubstituteImplementations(c2))

  override def toOpenCL(block: Block, ocl: ToOpenCL): Block = {
    ToOpenCL.cmd(c1, block, ocl)
    ToOpenCL.cmd(c2, block, ocl)
  }

  override def prettyPrint: String = s"${PrettyPrinter(c1)} ; ${PrettyPrinter(c2)}"

}
