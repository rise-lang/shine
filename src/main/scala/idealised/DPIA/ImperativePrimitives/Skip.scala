package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.CodeGenerator
import idealised.DPIA.Phrases.{CommandPrimitive, GeneratableCommand, Phrase, VisitAndRebuild}
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._

import scala.xml.Elem

// not final because of DSL.typed.skip
case class Skip() extends CommandPrimitive with GeneratableCommand {

  override val `type`: CommandType = comm

  override def eval(s: Store): Store = s

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommandType] = this

  override def codeGen(gen: CodeGenerator)(env: gen.Environment): gen.Stmt = gen.primitiveCodeGen.codeGenSkip

  override def prettyPrint: String = "skip"

  override def xmlPrinter: Elem = <skip/>
}
