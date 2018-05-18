package idealised.OpenMP.CodeGeneration

import idealised.DPIA.{Nat, freshName}
import idealised.DPIA.DSL._
import idealised.C.CodeGeneration._
import idealised.C.AST._
import idealised.C.CodeGeneration.CodeGenerator.Environment
import idealised.DPIA.Phrases.{Identifier, Phrase}
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import lift.arithmetic.{NamedVar, RangeAdd}

import scala.collection.immutable

class PrimitivesToOpenMP extends PrimitivesToC {

  override val name: String = "OpenMP"

  override def codeGenParFor(n: Nat,
                             dt: DataType,
                             a: Phrase[AccType],
                             i: Identifier[ExpType],
                             o: Phrase[AccType],
                             p: Phrase[CommandType],
                             env: Environment)
                            (implicit gen: CodeGenerator): Stmt = {
    val i_ = freshName("i_")
    val range = RangeAdd(0, n, 1)
    val updatedGen = gen.updatedRanges(i_, range)

    val init = VarDecl(i_, Type.int, init = Some(ArithmeticExpr(0)))
    val cond = BinaryExpr(DeclRef(i_), BinaryOperator.<, ArithmeticExpr(n))
    val increment = idealised.C.AST.Assignment(DeclRef(i_), ArithmeticExpr(NamedVar(i_, range) + 1))

    Stmts(
      Code("#pragma omp parallel for"),
      ForLoop(DeclStmt(init), cond, increment,
        Block(immutable.Seq(updatedGen.cmd(Phrase.substitute(a `@` i, `for`=o, `in`=p), env + (i.name -> i_))))))
  }

}
