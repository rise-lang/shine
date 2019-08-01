package idealised.DPIA.Compilation

import idealised.C.CodeGeneration.{CodeGenerator => CCodeGenerator}
import idealised.DPIA.{LetNatIdentifier, Nat}
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommType, DataType, ExpType}

trait CodeGenerator[Environment, Path, Stmt, Expr, Decl, Ident, Type] {
  def name: String

  def generate(phrase: Phrase[CommType],
               topLevelDefinitions: scala.Seq[(LetNatIdentifier, Phrase[ExpType])],
               env: CCodeGenerator.Environment): (scala.Seq[Decl], Stmt)

  def cmd(phrase: Phrase[CommType], env: Environment): Stmt

  def acc(phrase: Phrase[AccType],
          env: Environment,
          ps: Path,
          C: Expr => Stmt): Stmt

  def exp(phrase: Phrase[ExpType],
          env: Environment,
          ps: Path,
          C: Expr => Stmt): Stmt

  def typ(dt: DataType): Type

  def genNat(n:Nat, env:Environment):Expr
}