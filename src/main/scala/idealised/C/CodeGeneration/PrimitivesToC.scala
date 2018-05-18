package idealised.C.CodeGeneration

import idealised.DPIA.DSL._
import idealised.DPIA.FunctionalPrimitives._
import idealised.DPIA.ImperativePrimitives._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.IndexData
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised._
import lift.arithmetic._
import idealised.C.PrimitiveCodeGen
import idealised.C.AST._
import idealised.C.CodeGeneration.CodeGenerator.Environment

import scala.collection.immutable

class PrimitivesToC extends PrimitiveCodeGen {

  override val name: String = "C"

  // ==== generating C Statements ==== //

  override def codeGenSkip: Stmt = Comment("skip")

  override def codeGenSeq(p1: Phrase[CommandType],
                          p2: Phrase[CommandType],
                          env: Environment)
                         (implicit gen: CodeGenerator): Stmt = {
    Stmts(gen.cmd(p1, env), gen.cmd(p2, env))
  }

  override def codeGenAssign(a: Phrase[AccType],
                             e: Phrase[ExpType],
                             env: Environment)
                            (implicit gen: CodeGenerator): Stmt = {
    Assignment(gen.acc(a, env, List()), gen.exp(e, env, List()))
  }

  override def codeGenNew(dt: DataType,
                          v: Identifier[ExpType x AccType],
                          p: Phrase[CommandType],
                          env: CodeGenerator.Environment)
                         (implicit gen: CodeGenerator): Stmt = {
    Block(immutable.Seq(
      DeclStmt(VarDecl(v.name, Type.fromDataType(dt))),
      gen.cmd( Phrase.substitute(Pair(π1(v), π2(v)), `for`=v, `in`=p),
        env + ( v.name -> v.name, v.name -> v.name ) )
    ))
  }

  override def codeGenFor(n: Nat,
                          i: Identifier[ExpType],
                          p: Phrase[CommandType],
                          env: Environment)
                         (implicit gen: CodeGenerator): Stmt = {
    val i_ = freshName("i_")
    val range = RangeAdd(0, n, 1)
    val updatedGen = gen.updatedRanges(i_, range)

    val init = VarDecl(i_, Type.int, init = Some(ArithmeticExpr(0)))
    val cond = BinaryExpr(DeclRef(i_), BinaryOperator.<, ArithmeticExpr(n))
    val increment = idealised.C.AST.Assignment(DeclRef(i_), ArithmeticExpr(NamedVar(i_, range) + 1))

    ForLoop(DeclStmt(init), cond, increment,
      Block(immutable.Seq(updatedGen.cmd(p, env + (i.name -> i_)))))
  }

  override def codeGenParFor(n: Nat,
                             dt: DataType,
                             a: Phrase[AccType],
                             i: Identifier[ExpType],
                             o: Phrase[AccType],
                             p: Phrase[CommandType],
                             env: Environment)
                            (implicit gen: CodeGenerator): Stmt = {
    // in C the parFor is implemented sequentially
    codeGenFor(n, i, Phrase.substitute(a `@` i, `for`=o, `in`=p), env)
  }

  // ==== generating blocks  ==== //

//  def codeGen(a: Assign, block: Block, gen: CodeGenerator): Block = {
//    block + gen.acc(a.lhs, gen.exp(a.rhs, gen), gen)
//  }
//
//  def codeGen(f: For, block: Block, gen: CodeGenerator): Block = {
//
//    val bodyE = Lifting.liftFunction(f.body)
//
//    f.n match {
//      case Cst(0) =>
//        block + Comment("iteration count is 0, no loop emitted")
//
//      case Cst(1) =>
//        val zero = Phrases.Literal(IndexData(0))
//        block ++ immutable.Seq(
//          Comment("iteration count is exactly 1, no loop emitted"),
//          gen.cmd(bodyE(zero), Block(Vector()), gen)
//        )
//
//      case _ =>
//        val name = freshName("i_")
//        val range = RangeAdd(0, f.n, 1)
//        val updatedGen = gen.updatedRanges(name, range)
//
//        val init = VarDecl(name, Type.int, init = Some(ArithmeticExpr(0)))
//
//        val cond = BinaryExpr(DeclRef(name), BinaryOperator.<, ArithmeticExpr(f.n))
//
//        val i = identifier(name, ExpType(IndexType(f.n)))
//
//        val increment = {
//          val v = NamedVar(name, range)
//          idealised.C.AST.Assignment(DeclRef(name), ArithmeticExpr(v + 1))
//        }
//
//        val body_ = updatedGen.cmd(bodyE(i), Block(), updatedGen)
//        block + ForLoop(DeclStmt(init), cond, increment, body_)
//    }
//  }
//
//  def codeGen(d: DoubleBufferFor, b1: Block, gen: CodeGenerator): Block = {
//    val ptrType = PointerType(Type.fromDataType(d.dt))
//
//    // in* = buffer1
//    val buffer1Name = d.buffer1 match {
//      case i: Identifier[VarType] => i.name
//      case _ => throw new Exception("This should not happen")
//    }
//    val in = identifier(freshName("in_"), ExpType(DPIA.Types.ArrayType(d.n, d.dt)))
//
//    val b2 = b1 + DeclStmt(VarDecl(in.name, ptrType, init = Some(DeclRef(buffer1Name))))
//
//    // out* = buffer2
//    val buffer2Name = d.buffer2 match {
//      case i: Identifier[VarType] => i.name
//      case _ => throw new Exception("This should not happen")
//    }
//    val out = identifier(freshName("out_"), AccType(DPIA.Types.ArrayType(d.n, d.dt)))
//
//    val b3 = b2 + DeclStmt(VarDecl(out.name, ptrType, init = Some(DeclRef(buffer2Name))))
//
//    // for ...
//    val loopVar = NamedVar(freshName("i_"))
//    val updatedGen = gen.updatedRanges(loopVar.name, RangeAdd(0, d.k, 1))
//
//    val init = VarDecl(loopVar.name, Type.int, init = Some(ArithmeticExpr(0)))
//
//    val cond = BinaryExpr(DeclRef(loopVar.name), BinaryOperator.<, ArithmeticExpr(d.k))
//
//    val increment = idealised.C.AST.Assignment(DeclRef(loopVar.name), ArithmeticExpr(loopVar + 1))
//
//    val bodyE = Lifting.liftNatDependentFunction(d.body)
//    val bodyEE = Lifting.liftFunction(bodyE(loopVar))
//    val bodyEEE = Lifting.liftFunction(bodyEE(out))
//
//    val nestedBlock = Block({
//      val tmp = NamedVar(freshName("tmp_"))
//      immutable.Seq(
//        // tmp = in
//        DeclStmt(VarDecl(tmp.name, ptrType, init = Some(DeclRef(in.name)))),
//        // in = out
//        idealised.C.AST.Assignment(DeclRef(in.name), DeclRef(out.name)),
//        // out = tmp
//        idealised.C.AST.Assignment(DeclRef(out.name), DeclRef(tmp.name))
//      )
//    })
//    val body_ = updatedGen.cmd(bodyEEE(in), nestedBlock, updatedGen)
//
//    val b4 = b3 + ForLoop(DeclStmt(init), cond, increment, body_)
//
//    // copy result to output
//    val CE = Lifting.liftFunction(d.C)(identifier(in.name, ExpType(DPIA.Types.ArrayType(d.m, d.dt))))
//    TypeChecker(CE)
//
//    b4 + updatedGen.cmd(CE, Block(immutable.Seq()), updatedGen)
//  }
//
//  def codeGen(n: New, block: Block, gen: CodeGenerator): Block = {
//    val v = NamedVar(n.f match {
//      case Lambda(x, _) => x.name
//      case _ => ???
//    })
//    val f_ = Lifting.liftFunction(n.f)
//    val v_ = identifier(v.name, n.f.t.inT)
//    gen.cmd(f_(v_), block + DeclStmt(VarDecl(v.name, Type.fromDataType(n.dt))), gen)
//  }
//
//  def codeGen(s: idealised.DPIA.ImperativePrimitives.Seq, b1: Block, gen: CodeGenerator): Block = {
//    val b2 = gen.cmd(s.c1, b1, gen)
//    gen.cmd(s.c2, b2, gen)
//  }
//
//  def codeGen(s: idealised.DPIA.ImperativePrimitives.Skip, block: Block, gen: CodeGenerator): Block = {
//    block + Comment("skip")
//  }
//
//  def codeGen(pf: ParFor, block: Block, gen: CodeGenerator): Block = {
//    // rewrite parallel for into sequential for
//    codeGen(For(pf.n, λ(exp"[idx(${pf.n})]")( i => pf.body(i)(pf.out `@` i) )), block, gen)
//  }
//
//  // ==== generating expressions  ==== //
//
//  def codeGen(g: Gather, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr = {
//    val i :: is = arrayAccess
//    val j = OperationalSemantics.evalIndexExp(g.idxF(i))
//    gen.exp(g.array, gen, dt, j :: is, tupleAccess)
//  }
//
//  def codeGen(j: Join, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr = {
//    val i :: is = arrayAccess
//    gen.exp(j.array, gen, dt, (i / j.m) :: (i % j.m) :: is, tupleAccess)
//  }
//
//  def codeGen(s: Split, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr = {
//    val i :: j :: is = arrayAccess
//    gen.exp(s.array, gen, dt, ((i * s.n) + j) :: is, tupleAccess)
//  }
//
//  def codeGen(z: Zip, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr = {
//    val xj :: xs = tupleAccess
//
//    if (xj == Cst(1)) {
//      return gen.exp(z.e1, gen, dt, arrayAccess, xs)
//    }
//
//    if (xj == Cst(2)) {
//      return gen.exp(z.e2, gen, dt, arrayAccess, xs)
//    }
//
//    throw new Exception("This should not happen")
//  }
//
//  def codeGen(u: Unzip, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr = {
//    ???
//  }
//
//  def codeGen(f: Fst, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr = {
//    gen.exp(f.record, gen, dt, arrayAccess, 1 :: tupleAccess)
//  }
//
//  def codeGen(i: Idx, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr = {
//
//    val idx: ArithExpr = gen.exp(i.index, gen) match {
//      case DeclRef(name) => NamedVar(name, gen.ranges(name))
//      case ArithmeticExpr(ae) => ae
//      case _ => throw new Exception("This should not happen")
//    }
//
//    gen.exp(i.array, gen, dt, idx :: arrayAccess, tupleAccess)
//  }
//
// def codeGen(r: Record, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr = ???
//
//  def codeGen(s: Snd, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr = {
//    gen.exp(s.record, gen, dt, arrayAccess, 2 :: tupleAccess)
//  }
//
//  def codeGen(t: TruncExp, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr = {
//    gen.exp(t.array, gen, dt, arrayAccess, tupleAccess)
//  }
//
//  def codeGen(l: DPIA.Phrases.Literal, gen: CodeGenerator, dt: DataType, arrayAccess: List[ArithExpr], tupleAccess: List[ArithExpr]): Expr = {
//    import OperationalSemantics._
//
//    l.d match {
//      case i: IntData => idealised.C.AST.Literal(i.i.toString)
//      case b: BoolData => idealised.C.AST.Literal(b.b.toString)
//      case f: FloatData => idealised.C.AST.Literal(f.f.toString)
//      case i: IndexData => ArithmeticExpr(i.n)
//      case ad: ArrayData =>
//        val nestedLiteral = DPIA.Phrases.Literal(ad.a(0))
//        gen.exp(nestedLiteral, gen, nestedLiteral.d.dataType, arrayAccess.tail, tupleAccess)
//    }
//  }
//
//  // ==== generating var refs  ==== //
//
//  def codeGen(r: RecordAcc1, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr = {
//    gen.acc(r.record, value, gen, dt, arrayAccess, 1 :: tupleAccess)
//  }
//
//  def codeGen(i: IdxAcc, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr = {
//    val idx: ArithExpr = gen.exp(i.index, gen) match {
//      case DeclRef(name) => NamedVar(name, gen.ranges(name))
//      case ArithmeticExpr(ae) => ae
//    }
//
//    gen.acc(i.array, value, gen, dt, idx :: arrayAccess, tupleAccess)
//  }
//
//  def codeGen(join: JoinAcc, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr = {
//    val i :: j :: is = arrayAccess
//
//    gen.acc(join.array, value, gen, dt, ((i * join.m) + j) :: is, tupleAccess)
//  }
//
//  def codeGen(r: RecordAcc2, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr = {
//    gen.acc(r.record, value, gen, dt, arrayAccess, 2 :: tupleAccess)
//  }
//
//  def codeGen(s: SplitAcc, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr = {
//    val i :: is = arrayAccess
//    gen.acc(s.array, value, gen, dt, (i / s.n) :: (i % s.n) :: is, tupleAccess)
//  }
//
//  def codeGen(t: TruncAcc, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr = {
//    gen.acc(t.array, value, gen, dt, arrayAccess, tupleAccess)
//  }
//
//  def codeGen(s: ScatterAcc, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr = {
//    val i :: is = arrayAccess
//
//    val j = OperationalSemantics.evalIndexExp(s.idxF(i))
//
//    gen.acc(s.array, value, gen, dt, j :: is, tupleAccess)
//  }
//
//  def codeGen(u: UnzipAcc, value: Expr, gen: CodeGenerator, dt: DataType, arrayAccess: List[Nat], tupleAccess: List[Nat]): Expr = {
//    ???
//  }
}
