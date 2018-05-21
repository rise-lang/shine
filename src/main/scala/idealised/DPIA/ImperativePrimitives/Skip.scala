package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.CodeGenerator
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._

import scala.xml.Elem

// not final because of DSL.typed.skip
case class Skip() extends CommandPrimitive with GeneratableCommand {

  override val `type`: CommandType = comm

  override def eval(s: Store): Store = s

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommandType] = this

  override def codeGen[Environment, Path, Stmt, Expr, Decl](gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl])(env: Environment): Stmt = gen.codeGenSkip

  override def prettyPrint: String = "skip"

  override def xmlPrinter: Elem = <skip/>
}
