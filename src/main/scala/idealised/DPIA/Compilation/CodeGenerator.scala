package idealised.DPIA.Compilation

import idealised.DPIA.Phrases.{Identifier, Phrase}
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA.{Nat, x}

trait CodeGenerator {
  type Environment
  type Path

  type Stmt
  type Decl
  type Expr

  def generate: (Seq[Decl], Stmt)

  def primitiveCodeGen: PrimitiveCodeGen[Environment, Path, Stmt, Expr]

  def cmd(phrase: Phrase[CommandType], env: Environment): Stmt

  def acc(phrase: Phrase[AccType], env: Environment, ps: Path): Expr

  def exp(phrase: Phrase[ExpType], env: Environment, ps: Path): Expr
}

trait PrimitiveCodeGen[Environment, Path, Stmt, Expr] {
  def name: String

  def codeGenSkip: Stmt

  def codeGenSeq(p1: Phrase[CommandType],
                 p2: Phrase[CommandType],
                 env: Environment,
                 gen: CodeGenerator): Stmt

  def codeGenAssign(a: Phrase[AccType],
                    e: Phrase[ExpType],
                    env: Environment,
                    gen: CodeGenerator): Stmt

  def codeGenNew(dt: DataType,
                 v: Identifier[ExpType x AccType],
                 p: Phrase[CommandType],
                 env: Environment,
                 gen: CodeGenerator): Stmt

  def codeGenFor(n: Nat,
                 i: Identifier[ExpType],
                 p: Phrase[CommandType],
                 env: Environment,
                 gen: CodeGenerator): Stmt

  def codeGenParFor(n: Nat,
                    dt: DataType,
                    a: Phrase[AccType],
                    i: Identifier[ExpType],
                    o: Phrase[AccType],
                    p: Phrase[CommandType],
                    env: Environment,
                    gen: CodeGenerator): Stmt

  def codeGenIdxAcc(i: Phrase[ExpType],
                    a: Phrase[AccType],
                    env: Environment,
                    ps: Path,
                    gen: CodeGenerator): Expr
}