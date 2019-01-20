package idealised.DPIA.Compilation

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}

trait CodeGenerator[Environment, Path, Stmt, Expr, Decl, Ident, Type] {
  def name: String

  def generate(phrase: Phrase[CommandType], env: Environment): (Seq[Decl], Stmt)

  def cmd(phrase: Phrase[CommandType], env: Environment): Stmt

  def acc(phrase: Phrase[AccType], env: Environment, ps: Path): Expr

  def exp(phrase: Phrase[ExpType], env: Environment, ps: Path): Expr

  def typ(dt: DataType): Type
}