package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.CodeGenerator
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class ForNat(n: Nat,
                        body: Phrase[`(nat)->`[CommandType]])
  extends CommandPrimitive with GeneratableCommand {

  override val `type`: CommandType = {
    val k = body.t.x
    (n: Nat) -> (body :: t"($k:nat) -> comm") -> comm
  }
  override def eval(s: Store): Store = ???


  override def codeGen[Environment, Path, Stmt, Expr, Decl, Ident](gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl, Ident])(env: Environment): Stmt = {
    body match {
      case NatDependentLambda(i, p) => gen.codeGenForNat(n, i, p, env, gen)
      case _ => error(s"Expected lambda")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    ForNat(fun(n), VisitAndRebuild(body, fun))
  }

  override def prettyPrint: String = s"(forNat 0..$n ${PrettyPhrasePrinter(body)})"

  override def xmlPrinter: Elem =
    <forNat n={ToString(n)}>
      {Phrases.xmlPrinter(body)}
    </forNat>
}
