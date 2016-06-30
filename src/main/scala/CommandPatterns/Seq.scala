package CommandPatterns

import Core._
import Core.OperationalSemantics._
import Compiling.SubstituteImplementations
import opencl.generator.OpenCLAST.Block

import scala.xml.Elem

case class Seq(c1: Phrase[CommandType],
               c2: Phrase[CommandType])
  extends CommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    c1.t =?= comm
    c2.t =?= comm
    comm
  }

  override def eval(s: Store): Store = {
    val s1 = OperationalSemantics.eval(s, c1)
    OperationalSemantics.eval(s1, c2)
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    Seq(VisitAndRebuild(c1, fun), VisitAndRebuild(c2, fun))
  }

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] =
    Seq(SubstituteImplementations(c1, env), SubstituteImplementations(c2, env))

  override def toOpenCL(block: Block, env: ToOpenCL.Environment): Block = {
    ToOpenCL.cmd(c1, block, env)
    ToOpenCL.cmd(c2, block, env)
  }

  override def prettyPrint: String =
    s"(${PrettyPrinter(c1)}; ${PrettyPrinter(c2)})"

  override def xmlPrinter: Elem =
    <seq>
      <c1>{Core.xmlPrinter(c1)}</c1>
      <c2>{Core.xmlPrinter(c2)}</c2>
    </seq>
}
