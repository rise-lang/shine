package idealised.OpenCL.CodeGeneration

import idealised.C
import idealised.C.AST.Decl
import idealised.C.CodeGeneration.{CodeGenerator => CCodeGenerator}
import idealised.DPIA.{Nat, freshName}
import idealised.DPIA.Phrases.{Identifier, Phrase}
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA.DSL._
import lift.arithmetic
import lift.arithmetic._

import scala.collection.{immutable, mutable}

object CodeGenerator {
  type Environment = CCodeGenerator.Environment
  type Path = CCodeGenerator.Path
  type Expr = C.AST.Expr
  type Decl = C.AST.Decl
  type Stmt = C.AST.Stmt

  def apply(p: Phrase[CommandType], env: Environment): CodeGenerator =
    new CodeGenerator(p, env)
}

class CodeGenerator(override val p: Phrase[CommandType],
                    override val env: CCodeGenerator.Environment)
  extends CCodeGenerator(p, env, mutable.ListBuffer[Decl](), immutable.Map[String, arithmetic.Range]())
{
  override def cmd(phrase: Phrase[CommandType], env: Environment): Stmt = super.cmd(phrase, env)

  override def acc(phrase: Phrase[AccType], env: Environment, path: Path): Expr = super.acc(phrase, env, path)

  override def exp(phrase: Phrase[ExpType], env: Environment, path: Path): Expr = super.exp(phrase, env, path)
}
