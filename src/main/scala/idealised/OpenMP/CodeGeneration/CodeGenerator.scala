package idealised.OpenMP.CodeGeneration

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

  def apply(p: Phrase[CommandType], env: CCodeGenerator.Environment): CodeGenerator =
    new CodeGenerator(p, env)
}

class CodeGenerator(override val p: Phrase[CommandType],
                    override val env: CCodeGenerator.Environment)
  extends CCodeGenerator(p, env, mutable.ListBuffer[Decl](), immutable.Map[String, arithmetic.Range]())
{
  override def codeGenParFor(n: Nat,
                             dt: DataType,
                             a: Phrase[AccType],
                             i: Identifier[ExpType],
                             o: Phrase[AccType],
                             p: Phrase[CommandType],
                             env: Environment,
                             gen: CodeGenerator.this.type): Stmt = {
    val i_ = freshName("i_")
    val range = RangeAdd(0, n, 1)
    val updatedGen = gen.updatedRanges(i_, range)

    val init = C.AST.VarDecl(i_, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
    val cond = C.AST.BinaryExpr(C.AST.DeclRef(i_), C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
    val increment = idealised.C.AST.Assignment(C.AST.DeclRef(i_), C.AST.ArithmeticExpr(NamedVar(i_, range) + 1))

    C.AST.Stmts(
      C.AST.Code("#pragma omp parallel for"),
      C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
        C.AST.Block(immutable.Seq(updatedGen.cmd(Phrase.substitute(a `@` i, `for`=o, `in`=p), env + (i.name -> i_))))))
  }
}
