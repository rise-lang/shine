package idealised.C

import idealised.C.AST.{Expr, Stmt}
import idealised.DPIA.{Nat, x}
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.C.CodeGeneration.CodeGenerator
import idealised.DPIA.Phrases.{Identifier, Phrase}

trait GeneratableExp {
  def codeGen(gen: CodeGenerator)(env: CodeGenerator.Environment, path: CodeGenerator.Path): Expr
}

trait PrimitiveCodeGen {
  def name: String

  def codeGenSkip: Stmt

  def codeGenSeq(p1: Phrase[CommandType], p2: Phrase[CommandType], env: CodeGenerator.Environment)(implicit gen: CodeGenerator): Stmt

  def codeGenAssign(a: Phrase[AccType], e: Phrase[ExpType], env: CodeGenerator.Environment)(implicit gen: CodeGenerator): Stmt

  def codeGenNew(dt: DataType, v: Identifier[ExpType x AccType], p: Phrase[CommandType], env: CodeGenerator.Environment)(implicit gen: CodeGenerator): Stmt

  def codeGenFor(n: Nat,
                 i: Identifier[ExpType],
                 p: Phrase[CommandType],
                 env: CodeGenerator.Environment)
                (implicit gen: CodeGenerator): Stmt

  def codeGenParFor(n: Nat,
                    dt: DataType,
                    a: Phrase[AccType],
                    i: Identifier[ExpType],
                    o: Phrase[AccType],
                    p: Phrase[CommandType],
                    env: CodeGenerator.Environment)
                   (implicit gen: CodeGenerator): Stmt


//  def codeGen(a: Assign, block: Block, gen: CodeGenerator): Block
//
//  def codeGen(f: For, block: Block, gen: CodeGenerator): Block
//
//  def codeGen(d: DoubleBufferFor, block: Block, gen: CodeGenerator): Block
//
//  def codeGen(n: New, block: Block, gen: CodeGenerator): Block
//
//  def codeGen(s: DPIA.ImperativePrimitives.Seq, block: Block, gen: CodeGenerator): Block
//
//  def codeGen(s: DPIA.ImperativePrimitives.Skip, block: Block, gen: CodeGenerator): Block
//
//  def codeGen(pf: ParFor, block: Block, gen: CodeGenerator): Block

//  def codeGen(g: Gather, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr
//
//  def codeGen(j: Join, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr
//
//  def codeGen(s: Split, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr
//
//  def codeGen(z: Zip, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr
//
//  def codeGen(u: Unzip, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr
//
//  def codeGen(f: Fst, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr
//
//  def codeGen(i: Idx, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr
//
//  def codeGen(r: Record, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr
//
//  def codeGen(s: Snd, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr
//
//  def codeGen(t: TruncExp, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr
//
//  def codeGen(l: DPIA.Phrases.Literal, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr
//
//  def codeGen(r: RecordAcc1, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr
//
//  def codeGen(i: IdxAcc, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr
//
//  def codeGen(join: JoinAcc, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr
//
//  def codeGen(r: RecordAcc2, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr
//
//  def codeGen(s: SplitAcc, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr
//
//  def codeGen(t: TruncAcc, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr
//
//  def codeGen(s: ScatterAcc, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr
//
//  def codeGen(u: UnzipAcc, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr
}

