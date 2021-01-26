package shine.DPIA.Compilation

import shine.C
import shine.DPIA.Phrases.{Identifier, Phrase}
import shine.DPIA.Types.{AccType, BasePhraseType, CommType, DataType, ExpType, PhraseType}
import shine.DPIA.{->:, LetNatIdentifier, Nat, VarType}

import scala.collection.immutable

trait CodeGenerator {
  type Environment = CodeGenerator.Environment

  type Path
  type Stmt
  type Expr
  type Decl
  type Ident
  type Type

  def name: String

  def translationContext: TranslationContext

  def generate(topLevelDefinitions: scala.Seq[(LetNatIdentifier, Phrase[ExpType])],
               env: Environment): Phrase[CommType] => (scala.Seq[Decl], Stmt)

  def cmd(env: Environment): Phrase[CommType] => Stmt

  def acc(env: Environment,
          ps: Path,
          C: Expr => Stmt): Phrase[AccType] => Stmt

  def exp(env: Environment,
          ps: Path,
          C: Expr => Stmt): Phrase[ExpType] => Stmt

  def typ(dt: DataType): Type

  def generateAccess(dt: DataType,
                     expr: Expr,
                     path: Path,
                     env: Environment,
                     C: Expr => Stmt): Stmt

  def genNat(n:Nat, env:Environment, cont:Expr => Stmt): Stmt
}

object CodeGenerator {
  final case class
    Environment(identEnv: immutable.Map[Identifier[_ <: BasePhraseType], C.AST.DeclRef],
                varEnv: immutable.Map[Identifier[VarType], C.AST.DeclRef],
                commEnv: immutable.Map[Identifier[CommType], C.AST.Stmt],
                contEnv: immutable.Map[Identifier[ExpType ->: CommType],
                                       Phrase[ExpType] => Environment => C.AST.Stmt],
                letNatEnv: immutable.Map[LetNatIdentifier, Phrase[PhraseType]]) {
    def updatedIdentEnv(kv: (Identifier[_ <: BasePhraseType], C.AST.DeclRef)): Environment =
      this.copy(identEnv = identEnv + kv)

    def updatedVarEnv(kv: (Identifier[VarType], C.AST.DeclRef)): Environment =
      this.copy(varEnv = varEnv + kv)

    def updatedCommEnv(kv: (Identifier[CommType], C.AST.Stmt)): Environment =
      this.copy(commEnv = commEnv + kv)

    def updatedContEnv(kv: (Identifier[ExpType ->: CommType], Phrase[ExpType] => Environment => C.AST.Stmt)
                      ): Environment = this.copy(contEnv = contEnv + kv)

    def updatedNatEnv(kv: (LetNatIdentifier, Phrase[PhraseType])): Environment =
      this.copy(letNatEnv = this.letNatEnv + kv)
  }
}
