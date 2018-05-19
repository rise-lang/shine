package idealised.DPIA.Compilation

import idealised.DPIA.Phrases.{Identifier, Phrase}
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA.{Nat, x}
import idealised.SurfaceLanguage

trait CodeGenerator[Environment, Path, Stmt, Expr, Decl] {
  def generate: (Seq[Decl], Stmt)

  def primitiveCodeGen: PrimitiveCodeGen[Environment, Path, Stmt, Expr, Decl]

  def cmd(phrase: Phrase[CommandType], env: Environment): Stmt

  def acc(phrase: Phrase[AccType], env: Environment, ps: Path): Expr

  def exp(phrase: Phrase[ExpType], env: Environment, ps: Path): Expr
}

trait PrimitiveCodeGen[Environment, Path, Stmt, Expr, Decl] {
  def name: String

  def codeGenSkip: Stmt

  def codeGenSeq(p1: Phrase[CommandType],
                 p2: Phrase[CommandType],
                 env: Environment,
                 gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl]): Stmt

  def codeGenAssign(a: Phrase[AccType],
                    e: Phrase[ExpType],
                    env: Environment,
                    gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl]): Stmt

  def codeGenNew(dt: DataType,
                 v: Identifier[ExpType x AccType],
                 p: Phrase[CommandType],
                 env: Environment,
                 gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl]): Stmt

  def codeGenFor(n: Nat,
                 i: Identifier[ExpType],
                 p: Phrase[CommandType],
                 env: Environment,
                 gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl]): Stmt

  def codeGenParFor(n: Nat,
                    dt: DataType,
                    a: Phrase[AccType],
                    i: Identifier[ExpType],
                    o: Phrase[AccType],
                    p: Phrase[CommandType],
                    env: Environment,
                    gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl]): Stmt

  def codeGenIdxAcc(i: Phrase[ExpType],
                    a: Phrase[AccType],
                    env: Environment,
                    ps: Path,
                    gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl]): Expr

  def codeGenLiteral(d: OperationalSemantics.Data): Expr

  def codeGenUnaryOp(op: SurfaceLanguage.Operators.Unary.Value,
                     e: Expr): Expr

  def codeGenBinaryOp(op: SurfaceLanguage.Operators.Binary.Value,
                      e1: Expr, e2: Expr): Expr

  def codeGenIdx(i: Phrase[ExpType],
                 e: Phrase[ExpType],
                 env: Environment,
                 ps: Path,
                 gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl]): Expr

  def generateAccess(identifier: String, paths: Path): Expr
}