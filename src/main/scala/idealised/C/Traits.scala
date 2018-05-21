//package idealised.C
//
//import idealised.DPIA
//import idealised.C.AST.{Block, Expr}
//import idealised.DPIA.Nat
//import idealised.DPIA.Types.DataType
//import idealised.DPIA.FunctionalPrimitives._
//import idealised.DPIA.ImperativePrimitives._
//import idealised.C.CodeGeneration.CodeGenerator
//
//import lift.arithmetic._
//
//import scala.collection.immutable.List
//
//trait GeneratableComm {
//  def codeGen(block: Block, gen: CodeGenerator): Block
//}
//
//trait GeneratableExp {
//  def codeGen(gen: CodeGenerator): Expr
//}
//
//trait ViewExp {
//  def codeGen(gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr
//}
//
//trait ViewAcc {
//  def codeGen(gen: CodeGenerator, value: Expr, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]):  Expr
//}
//
//trait PrimitiveCodeGen {
//  def name: String
//
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
//
//  def codeGen(pf: ParForVec, block: Block, gen: CodeGenerator): Block
//
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
//  def codeGen(i: IdxVec, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr
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
//  def codeGen(i: IdxVecAcc, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr
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
//}
//
