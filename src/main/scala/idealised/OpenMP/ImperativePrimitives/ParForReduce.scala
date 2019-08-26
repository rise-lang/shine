package idealised.OpenMP.ImperativePrimitives

import idealised.C.AST._
import idealised.C.CodeGeneration.CodeGenerator
import idealised.DPIA.DSL.{identifier, _}
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import lift.arithmetic.{Cst, RangeAdd}

case class ParForReduce() {}
// Can't inherit from AbstractParFor
//case class ParForReduce(override val n: Nat,
//                        override val dt: DataType,
//                        override val out: Phrase[AccType],
//                        override val body: Phrase[ExpType -> (AccType -> CommandType)])
//  extends AbstractParFor(n, dt, out, body) with GeneratableComm {
//
//  override def makeParFor:
//    (Nat, DataType, Phrase[AccType], Phrase[->[ExpType, ->[AccType, CommandType]]]) => ParFor = ParFor
//
//  override def codeGen(block: Block, gen: CodeGenerator): Block = {
//    val name: String = freshName("i_")
//    val start: Nat = Cst(0)
//    val step: Nat = Cst(1)
//    val stop: Nat = n
//
//    val range = RangeAdd(start, stop, step)
//    val updatedGen = gen.updatedRanges(name, range)
//
//    val i = identifier(name, exp"[idx($stop)]")
//    val body_ = Lifting.liftFunction( Lifting.liftFunction(body)(i) )
//    val out_at_i = out `@` i
//    TypeChecker(out_at_i)
//
//    val initDecl = VarDecl(name, Type.int, init = Some(ArithmeticExpr(start)))
//
//    val cond = BinaryExpr(DeclRef(name), BinaryOperator.<, ArithmeticExpr(stop))
//
//    val increment = {
//      val v = NamedVar(name, range)
//      idealised.C.AST.Assignment(DeclRef(v.name), ArithmeticExpr(v + 1))
//    }
//
//    val bodyBlock = (b: Block) => updatedGen.cmd(body_(out_at_i), b, updatedGen)
//
//    range.numVals match {
//      case Cst(0) =>
//        block + Comment("iteration count is 0, no loop emitted")
//
//      case Cst(1) =>
//        block ++ Seq(
//          Comment("iteration count is exactly 1, no loop emitted"),
//          bodyBlock(Block(Seq(DeclStmt(initDecl)))))
//
//      case _ =>
//        if ( (range.start.min.min == Cst(0) && range.stop == Cst(1))
//          || (range.numVals.min == Cst(0) && range.numVals.max == Cst(1)) ) {
//
//          block ++ Seq(
//            Comment("iteration count is 1 or less, no loop emitted"),
//            Block(Seq(
//              DeclStmt(initDecl),
//              IfThenElse(
//                BinaryExpr(ArithmeticExpr(start), BinaryOperator.<, ArithmeticExpr(stop)),
//                bodyBlock(Block()),
//                None))))
//        } else {
//          block ++ Seq(
//            Code("#pragma omp parallel for"),
//            ForLoop(DeclStmt(initDecl), cond, increment, bodyBlock(Block())))
//        }
//
//    }
//  }
//}
