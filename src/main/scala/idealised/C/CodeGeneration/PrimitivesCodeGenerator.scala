package idealised.C.CodeGeneration

import idealised.DPIA.DSL._
import idealised.DPIA.FunctionalPrimitives._
import idealised.DPIA.ImperativePrimitives.{Seq => ISeq, _}
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.IndexData
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised._
import lift.arithmetic._
import idealised.C.AST._

object PrimitivesCodeGenerator {

  // ==== generating blocks  ==== //

  def toC(a: Assign, block: Block, env: CodeGenerator.Environment): Block = {
    block + CodeGenerator.acc(a.lhs, CodeGenerator.exp(a.rhs, env), env)
  }

  def toC(f: For, block: Block, env: CodeGenerator.Environment): Block = {

    val bodyE = Lifting.liftFunction(f.body)

    f.n match {
      case Cst(0) =>
        block + Comment("iteration count is 0, no loop emitted")

      case Cst(1) =>
        val zero = Phrases.Literal(IndexData(0), IndexType(1))
        block ++ Seq(
          Comment("iteration count is exactly 1, no loop emitted"),
          CodeGenerator.cmd(bodyE(zero), Block(Vector()), env)
        )

      case _ =>
        val name = freshName("i_")
        val range = RangeAdd(0, f.n, 1)

        val init = VarDecl(name, Type.int, init = Some(ArithmeticExpr(0)))

        val cond = BinaryExpr(DeclRef(name), BinaryOperator.<, ArithmeticExpr(f.n))

        val i = identifier(name, ExpType(IndexType(f.n)))

        val v = NamedVar(name, range)
        val increment = idealised.C.AST.Assignment(DeclRef(v.name), ArithmeticExpr(v + 1))

        val body_ = CodeGenerator.cmd(bodyE(i), Block(), env.updatedRanges(name, range))
        block + ForLoop(DeclStmt(init), cond, increment, body_)
    }
  }

  def toC(d: DoubleBufferFor, b1: Block, env: CodeGenerator.Environment): Block = {
    val ptrType = PointerType(Type.fromDataType(d.dt))

    // in* = buffer1
    val buffer1Name = d.buffer1 match {
      case i: Identifier[VarType] => i.name
      case _ => throw new Exception("This should not happen")
    }
    val in = identifier(freshName("in_"), ExpType(DPIA.Types.ArrayType(d.n, d.dt)))

    val b2 = b1 + DeclStmt(VarDecl(in.name, ptrType, init = Some(DeclRef(buffer1Name))))

    // out* = buffer2
    val buffer2Name = d.buffer2 match {
      case i: Identifier[VarType] => i.name
      case _ => throw new Exception("This should not happen")
    }
    val out = identifier(freshName("out_"), AccType(DPIA.Types.ArrayType(d.n, d.dt)))

    val b3 = b2 + DeclStmt(VarDecl(out.name, ptrType, init = Some(DeclRef(buffer2Name))))

    // for ...
    val loopVar = NamedVar(freshName("i_"))
    val updatedEnv = env.updatedRanges(loopVar.name, RangeAdd(0, d.k, 1))

    val init = VarDecl(loopVar.name, Type.int, init = Some(ArithmeticExpr(0)))

    val cond = BinaryExpr(DeclRef(loopVar.name), BinaryOperator.<, ArithmeticExpr(d.k))

    val increment = idealised.C.AST.Assignment(DeclRef(loopVar.name), ArithmeticExpr(loopVar + 1))

    val bodyE = Lifting.liftNatDependentFunction(d.body)
    val bodyEE = Lifting.liftFunction(bodyE(loopVar))
    val bodyEEE = Lifting.liftFunction(bodyEE(out))

    val nestedBlock = Block({
      val tmp = NamedVar(freshName("tmp_"))
      Seq(
        // tmp = in
        DeclStmt(VarDecl(tmp.name, ptrType, init = Some(DeclRef(in.name)))),
        // in = out
        idealised.C.AST.Assignment(DeclRef(in.name), DeclRef(out.name)),
        // out = tmp
        idealised.C.AST.Assignment(DeclRef(out.name), DeclRef(tmp.name))
      )
    })
    val body_ = CodeGenerator.cmd(bodyEEE(in), nestedBlock, updatedEnv)

    val b4 = b3 + ForLoop(DeclStmt(init), cond, increment, body_)

    // copy result to output
    val CE = Lifting.liftFunction(d.C)(identifier(in.name, ExpType(DPIA.Types.ArrayType(d.m, d.dt))))
    TypeChecker(CE)

    b4 + CodeGenerator.cmd(CE, Block(Seq()), updatedEnv)
  }

  def toC(n: New, block: Block, env: CodeGenerator.Environment): Block = {
    val v = NamedVar(n.f match {
      case Lambda(x, _) => x.name
      case _ => ???
    })
    val f_ = Lifting.liftFunction(n.f)
    val v_ = identifier(v.name, n.f.t.inT)
    CodeGenerator.cmd(f_(v_), block + DeclStmt(VarDecl(v.name, Type.fromDataType(n.dt))), env)
  }

  def toC(s: idealised.DPIA.ImperativePrimitives.Seq,
          b1: Block,
          env: CodeGenerator.Environment): Block = {
    val b2 = CodeGenerator.cmd(s.c1, b1, env)
    CodeGenerator.cmd(s.c2, b2, env)
  }

  def toC(s: idealised.DPIA.ImperativePrimitives.Skip,
          block: Block,
          env: CodeGenerator.Environment): Block = {
    block + Comment("skip")
  }

  def toC(pf: ParFor,
          block: Block,
          env: CodeGenerator.Environment): Block = {
    // rewrite parallel for into sequential for
    toC(For(pf.n, λ(exp"[idx(${pf.n})]")( i => pf.body(i)(pf.out `@` i) )), block, env)
  }

  // ==== generating expressions  ==== //

  def toC(g: Gather,
               env: CodeGenerator.Environment,
               arrayAccess: List[ArithExpr],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expr = {
    val i :: is = arrayAccess
    val j = OperationalSemantics.evalIndexExp(g.idxF(i))
    CodeGenerator.exp(g.array, env, dt, j :: is, tupleAccess)
  }

  def toC(j: Join,
               env: CodeGenerator.Environment,
               arrayAccess: List[ArithExpr],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expr = {
    val i :: is = arrayAccess
    CodeGenerator.exp(j.array, env, dt, (i / j.m) :: (i % j.m) :: is, tupleAccess)
  }

  def toC(s: Split,
               env: CodeGenerator.Environment,
               arrayAccess: List[ArithExpr],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expr = {
    val i :: j :: is = arrayAccess
    CodeGenerator.exp(s.array, env, dt, ((i * s.n) + j) :: is, tupleAccess)
  }

  def toC(z: Zip,
               env: CodeGenerator.Environment,
               arrayAccess: List[ArithExpr],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expr = {
    val xj :: xs = tupleAccess

    if (xj == Cst(1)) {
      return CodeGenerator.exp(z.e1, env, dt, arrayAccess, xs)
    }

    if (xj == Cst(2)) {
      return CodeGenerator.exp(z.e2, env, dt, arrayAccess, xs)
    }

    throw new Exception("This should not happen")
  }

  def toC(u: Unzip,
               env: CodeGenerator.Environment,
               arrayAccess: List[ArithExpr],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expr = {
    ???
  }

  def toC(f: Fst,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[ArithExpr],
               tupleAccess: List[ArithExpr]): Expr = {
    CodeGenerator.exp(f.record, env, dt, arrayAccess, 1 :: tupleAccess)
  }

  def toC(i: Idx,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[ArithExpr],
               tupleAccess: List[ArithExpr]): Expr = {

    val idx: ArithExpr = CodeGenerator.exp(i.index, env) match {
      case DeclRef(name) => NamedVar(name, env.ranges(name))
      case ArithmeticExpr(ae) => ae
      case _ => throw new Exception("This should not happen")
    }

    CodeGenerator.exp(i.array, env, dt, idx :: arrayAccess, tupleAccess)
  }

  def toC(r: Record,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[ArithExpr],
               tupleAccess: List[ArithExpr]): Expr = ???

  def toC(s: Snd,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[ArithExpr],
               tupleAccess: List[ArithExpr]): Expr = {
    CodeGenerator.exp(s.record, env, dt, arrayAccess, 2 :: tupleAccess)
  }

  def toC(t: TruncExp,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[ArithExpr],
               tupleAccess: List[ArithExpr]): Expr = {
    CodeGenerator.exp(t.array, env, dt, arrayAccess, tupleAccess)
  }

  def toC(l: DPIA.Phrases.Literal,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[ArithExpr],
               tupleAccess: List[ArithExpr]): Expr = {
    import OperationalSemantics._

    l.dt match {
      case _: DPIA.Types.BasicType =>
        l.d match {
          case i: IntData => idealised.C.AST.Literal(i.i.toString)
          case b: BoolData => idealised.C.AST.Literal(b.b.toString)
          case f: FloatData => idealised.C.AST.Literal(f.f.toString)
          case i: IndexData => ArithmeticExpr(i.i)
        }
      case ct: DPIA.Types.ComposedType => (ct, l.d) match {
        case (at: DPIA.Types.ArrayType, ad: ArrayData) =>
          val nestedLiteral = DPIA.Phrases.Literal(ad.a(0), at.elemType)
          CodeGenerator.exp(nestedLiteral, env, at.elemType, arrayAccess.tail, tupleAccess)
        case (rt: DPIA.Types.RecordType, rd: RecordData) =>
          ???
      }
    }
  }

  // ==== generating var refs  ==== //

  def toC(r: RecordAcc1,
               value: Expr,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[Nat],
               tupleAccess: List[Nat]): Expr = {
    CodeGenerator.acc(r.record, value, env, dt, arrayAccess, 1 :: tupleAccess)
  }

  def toC(i: IdxAcc,
               value: Expr,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[Nat],
               tupleAccess: List[Nat]): Expr = {
    val idx: ArithExpr = CodeGenerator.exp(i.index, env) match {
      case DeclRef(name) => NamedVar(name, env.ranges(name))
      case ArithmeticExpr(ae) => ae
    }

    CodeGenerator.acc(i.array, value, env, dt, idx :: arrayAccess, tupleAccess)
  }

  def toC(join: JoinAcc,
               value: Expr,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[Nat],
               tupleAccess: List[Nat]): Expr = {
    val i :: j :: is = arrayAccess

    CodeGenerator.acc(join.array, value, env, dt, ((i * join.m) + j) :: is, tupleAccess)
  }

  def toC(r: RecordAcc2,
               value: Expr,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[Nat],
               tupleAccess: List[Nat]): Expr = {
    CodeGenerator.acc(r.record, value, env, dt, arrayAccess, 2 :: tupleAccess)
  }

  def toC(s: SplitAcc,
               value: Expr,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[Nat],
               tupleAccess: List[Nat]): Expr = {
    val i :: is = arrayAccess
    CodeGenerator.acc(s.array, value, env, dt, (i / s.n) :: (i % s.n) :: is, tupleAccess)
  }

  def toC(t: TruncAcc,
               value: Expr,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[Nat],
               tupleAccess: List[Nat]): Expr = {
    CodeGenerator.acc(t.array, value, env, dt, arrayAccess, tupleAccess)
  }

  def toC(s: ScatterAcc,
               value: Expr,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[Nat],
               tupleAccess: List[Nat]): Expr = {
    val i :: is = arrayAccess

    val j = OperationalSemantics.evalIndexExp(s.idxF(i))

    CodeGenerator.acc(s.array, value, env, dt, j :: is, tupleAccess)
  }

  def toC(u: UnzipAcc,
               value: Expr,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[Nat],
               tupleAccess: List[Nat]): Expr = {
    // TODO: FIX THIS
    ???
  }

}
