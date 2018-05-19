package idealised.DPIA.Compilation

import idealised.DPIA.Phrases.{Identifier, Phrase}
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA.{Nat, x}
import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.Primitives.ForeignFunctionDeclaration

trait CodeGenerator[Environment, Path, Stmt, Expr, Decl] {
  def name: String

  def generate: (Seq[Decl], Stmt)

  def cmd(phrase: Phrase[CommandType], env: Environment): Stmt

  def acc(phrase: Phrase[AccType], env: Environment, ps: Path): Expr

  def exp(phrase: Phrase[ExpType], env: Environment, ps: Path): Expr

  def codeGenSkip: Stmt

  def codeGenSeq(p1: Phrase[CommandType],
                 p2: Phrase[CommandType],
                 env: Environment,
                 gen: this.type): Stmt

  def codeGenAssign(a: Phrase[AccType],
                    e: Phrase[ExpType],
                    env: Environment,
                    gen: this.type): Stmt

  def codeGenNew(dt: DataType,
                 v: Identifier[ExpType x AccType],
                 p: Phrase[CommandType],
                 env: Environment,
                 gen: this.type): Stmt

  def codeGenFor(n: Nat,
                 i: Identifier[ExpType],
                 p: Phrase[CommandType],
                 env: Environment,
                 gen: this.type): Stmt

  def codeGenParFor(n: Nat,
                    dt: DataType,
                    a: Phrase[AccType],
                    i: Identifier[ExpType],
                    o: Phrase[AccType],
                    p: Phrase[CommandType],
                    env: Environment,
                    gen: this.type): Stmt

  def codeGenIdxAcc(i: Phrase[ExpType],
                    a: Phrase[AccType],
                    env: Environment,
                    ps: Path,
                    gen: this.type): Expr

  def codeGenLiteral(d: OperationalSemantics.Data): Expr

  def codeGenUnaryOp(op: SurfaceLanguage.Operators.Unary.Value,
                     e: Expr): Expr

  def codeGenBinaryOp(op: SurfaceLanguage.Operators.Binary.Value,
                      e1: Expr, e2: Expr): Expr

  def codeGenIdx(i: Phrase[ExpType],
                 e: Phrase[ExpType],
                 env: Environment,
                 ps: Path,
                 gen: this.type): Expr

  def codeGenForeignFunction(funDecl: ForeignFunctionDeclaration,
                             inTs: Seq[DataType],
                             outT: DataType,
                             args: Seq[Phrase[ExpType]],
                             env: Environment,
                             ps: Path,
                             gen: this.type): Expr

  def generateAccess(identifier: String, paths: Path): Expr
}