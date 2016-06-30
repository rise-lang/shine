package CommandPatterns

import Core._
import Core.OperationalSemantics._
import opencl.generator.OpenCLAST.Block
import Compiling.SubstituteImplementations

import scala.xml.Elem

case class Skip() extends CommandPattern {

  override def typeCheck(): CommandType = comm

  override def eval(s: Store): Store = s

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[CommandType] = this

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = this

  override def toOpenCL(b: Block, env: ToOpenCL.Environment): Block = b

  override def prettyPrint: String = "skip"

  override def xmlPrinter: Elem = <skip/>
}
