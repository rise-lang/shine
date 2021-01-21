package shine.DPIA.Compilation

import shine.C.CodeGeneration.{CodeGenerator => CCodeGenerator}

import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType}
import shine.DPIA.{LetNatIdentifier, Nat}

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

  def generateAccess(dt: DataType,
                     expr: Expr,
                     path: Path,
                     env: Environment,
                     C: Expr => Stmt): Stmt

  def genNat(n:Nat, env:Environment, cont:Expr => Stmt): Stmt
}