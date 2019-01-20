package idealised.OpenCL.CodeGeneration

import idealised.C.AST.Stmt
import idealised.DPIA.DSL._
import idealised.DPIA.FunctionalPrimitives._
import idealised.DPIA.ImperativePrimitives._
import idealised.DPIA.Phrases.{Identifier, Lambda, Phrase}
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.IndexData
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.ImperativePrimitives.OpenCLParFor
import idealised._
import lift.arithmetic._
import opencl.generator.OpenCLAST
import opencl.generator.OpenCLAST._
import opencl.ir.{PrivateMemory, PtrType}

//object PrimitivesCodeGenerator {
//  // ==== generating blocks  ==== //
//
//  def toOpenCL(a: Assign, block: Block, env: OpenCLOldCodeGenerator.Environment): Block = {
//    (block: Block) += OpenCLOldCodeGenerator.acc(a.lhs, OpenCLOldCodeGenerator.exp(a.rhs, env), env)
//      // AssignmentExpression(ToOpenCL.acc(a.lhs, env), ToOpenCL.exp(a.rhs, env))
//  }
//
//  def toOpenCL(f: For, block: Block, env: OpenCLOldCodeGenerator.Environment): Block = {
//    import opencl.generator.OpenCLAST._
//
//    val bodyE = Lifting.liftFunction(f.body)
//
//    f.n match {
//      case Cst(0) =>
//        (block: Block) +=
//          OpenCLAST.Comment("iteration count is 0, no loop emitted")
//
//      case Cst(1) =>
//        (block: Block) +=
//          OpenCLAST.Comment("iteration count is exactly 1, no loop emitted")
//        val zero = Phrases.Literal(IndexData(0))
//        (block: Block) += OpenCLOldCodeGenerator.cmd(bodyE(zero), Block(Vector()), env)
//
//      case _ =>
//        val name = freshName("i_")
//        val range = RangeAdd(0, f.n, 1)
//
//        val init = VarDecl(name, opencl.ir.Int,
//          init = ArithExpression(0),
//          addressSpace = opencl.ir.PrivateMemory)
//
//        val cond = CondExpression(VarRef(name),
//          ArithExpression(f.n),
//          CondExpression.Operator.<)
//
//        val i = identifier(name, ExpType(IndexType(f.n)))
//
//        val v = NamedVar(name, range)
//        val increment = AssignmentExpression(ArithExpression(v), ArithExpression(v + 1))
//
//        val body_ = OpenCLOldCodeGenerator.cmd(bodyE(i), Block(), env.updatedRanges(name, range))
//        (block: Block) += ForLoop(init, cond, increment, body_)
//    }
//
//    block
//  }
//
//  def toOpenCL(f: ForNat, block: Block, env: OpenCLOldCodeGenerator.Environment): Block = {
//    import opencl.generator.OpenCLAST._
//
//    val bodyE = Lifting.liftNatDependentFunction(f.body)
//
//    f.n match {
//      case Cst(0) =>
//        (block: Block) +=
//          OpenCLAST.Comment("iteration count is 0, no loop emitted")
//
//      case Cst(1) =>
//        (block: Block) +=
//          OpenCLAST.Comment("iteration count is exactly 1, no loop emitted")
//        (block: Block) += OpenCLOldCodeGenerator.cmd(bodyE(0), Block(Vector()), env)
//
//      case _ =>
//        val name = freshName("i_")
//        val range = RangeAdd(0, f.n, 1)
//
//        val init = VarDecl(name, opencl.ir.Int,
//          init = ArithExpression(0),
//          addressSpace = opencl.ir.PrivateMemory)
//
//        val cond = CondExpression(VarRef(name),
//          ArithExpression(f.n),
//          CondExpression.Operator.<)
//
//        val v = NamedVar(name, range)
//        val increment = AssignmentExpression(ArithExpression(v), ArithExpression(v + 1))
//
//        val body_ = OpenCLOldCodeGenerator.cmd(bodyE(v), Block(), env.updatedRanges(name, range))
//        (block: Block) += ForLoop(init, cond, increment, body_)
//    }
//
//    block
//  }
//
//  def toOpenCL(d: DoubleBufferFor, block: Block, env: OpenCLOldCodeGenerator.Environment): Block = {
//    import opencl.generator.OpenCLAST._
//
//    val oclAddressSpace = d.addressSpace.asInstanceOf[idealised.OpenCL.AddressSpace]
//
//    val ptrType =
//      new PtrType(DataType.scalarType(d.dt), OpenCL.AddressSpace.toOpenCL(oclAddressSpace))
//
//    // in* = buffer1
//    val buffer1Name = d.buffer1 match {
//      case i: Identifier[VarType] => i.name
//      case _ => throw new Exception("This should not happen")
//    }
//    val in = identifier(freshName("in_"), ExpType(ArrayType(d.n, d.dt)))
//
//    (block: Block) += VarDecl(in.name, ptrType,
//      init = VarRef(buffer1Name), addressSpace = opencl.ir.PrivateMemory)
//
//    // out* = buffer2
//    val buffer2Name = d.buffer2 match {
//      case i: Identifier[VarType] => i.name
//      case _ => throw new Exception("This should not happen")
//    }
//    val out = identifier(freshName("out_"), AccType(ArrayType(d.n, d.dt)))
//
//    (block: Block) += VarDecl(out.name, ptrType,
//      init = VarRef(buffer2Name), addressSpace = opencl.ir.PrivateMemory)
//
//    // for ...
//    val loopVar = NamedVar(freshName("i_"))
//    val updatedEnv = env.updatedRanges(loopVar.name, RangeAdd(0, d.k, 1))
//
//    val init = VarDecl(loopVar.name, opencl.ir.Int,
//      init = ArithExpression(0),
//      addressSpace = opencl.ir.PrivateMemory)
//
//    val cond = CondExpression(VarRef(loopVar.name),
//      ArithExpression(d.k),
//      CondExpression.Operator.<)
//
//    val increment = AssignmentExpression(
//      ArithExpression(loopVar), ArithExpression(loopVar + 1))
//
//    val bodyE = Lifting.liftNatDependentFunction(d.body)
//    val bodyEE = Lifting.liftFunction(bodyE(loopVar))
//    val bodyEEE = Lifting.liftFunction(bodyEE(out))
//
//    val nestedBlock = Block()
//    val body_ = OpenCLOldCodeGenerator.cmd(bodyEEE(in), nestedBlock, updatedEnv)
//
//    (block: Block) += ForLoop(init, cond, increment, body_)
//
//    val tmp = NamedVar(freshName("tmp_"))
//    // tmp = in
//    nestedBlock += VarDecl(tmp.name, ptrType,
//      init = VarRef(in.name), addressSpace = opencl.ir.PrivateMemory)
//
//    // in = out
//    nestedBlock +=
//      AssignmentExpression(
//        ArithExpression(NamedVar(in.name)),
//        ArithExpression(NamedVar(out.name)))
//
//    // out = tmp
//    nestedBlock +=
//      AssignmentExpression(
//        ArithExpression(NamedVar(out.name)),
//        ArithExpression(NamedVar(tmp.name)))
//
//    // copy result to output
//    val CE = Lifting.liftFunction(d.C)(identifier(in.name, ExpType(ArrayType(d.m, d.dt))))
//    TypeCheck(CE)
//    (block: Block) += OpenCLOldCodeGenerator.cmd(CE, Block(), updatedEnv)
//
//    block
//  }
//
//  def toOpenCL(n: New, block: Block, env: OpenCLOldCodeGenerator.Environment): Block = {
//    val v = NamedVar(n.f match {
//      case Lambda(x, _) => x.name
//      case _ => ???
//    })
//
//    if (n.addressSpace == OpenCL.PrivateMemory) {
////      val unrolledNames = unrollName(v.name, n.dt, collection.Seq())
////      val unrolldTypes = unrollType(n.dt, collection.Seq())
////      (unrolledNames zip unrolldTypes).foreach {
////        case (name, dt) => (block: Block) += VarDecl(name, DataType.toType(dt))
////      }
//      (block: Block) += VarDecl(v.name, DataType.toType(n.dt), addressSpace = PrivateMemory)
//    } else {
//      // TODO: throw exception
//      (block: Block) += Comment(s"new ${v.name} ${n.dt} ${n.addressSpace}")
//    }
//
//    val f_ = Lifting.liftFunction(n.f)
//    val v_ = identifier(v.name, n.f.t.inT)
//    OpenCLOldCodeGenerator.cmd(f_(v_), block, env)
//  }
//
//  def unrollName(name: String, dt: DataType, seq: collection.Seq[String]): collection.Seq[String] = {
//    dt match {
//      case _: BasicType => seq :+ name
//      case ct: ComposedType => ct match {
//        case ArrayType(size, et) =>
//          (0 until size.evalInt).foldLeft(seq){
//            (names, i) => unrollName(s"${name}_$i", et, names)
//          }
//        case rt: RecordType => ???
//      }
//      case _: DataTypeIdentifier => ???
//    }
//  }
//
//  def unrollType(dt: DataType, seq: collection.Seq[DataType]): collection.Seq[DataType] = {
//    dt match {
//      case _: BasicType => seq :+ dt
//      case ct: ComposedType => ct match {
//        case ArrayType(size, et) =>
//          (0 until size.evalInt).foldLeft(seq){
//            (types, _) => unrollType(et, types)
//          }
//        case rt: RecordType => ???
//      }
//      case _: DataTypeIdentifier => ???
//    }
//  }
//
//  def toOpenCL(s: idealised.DPIA.ImperativePrimitives.Seq,
//               block: Block,
//               env: OpenCLOldCodeGenerator.Environment): Block = {
//    OpenCLOldCodeGenerator.cmd(s.c1, block, env)
//    OpenCLOldCodeGenerator.cmd(s.c2, block, env)
//  }
//
//  def toOpenCL(s: idealised.DPIA.ImperativePrimitives.Skip,
//               block: Block,
//               env: OpenCLOldCodeGenerator.Environment): Block = {
//    (block: Block) += OpenCLAST.Comment("skip")
//  }
//
//  def toOpenCL(pf: ParFor,
//               block: Block,
//               env: OpenCLOldCodeGenerator.Environment): Block = {
//
//    // TODO: this is not really a par for if it is done sequentially ...
//    case class OpenCLParForSeq(override val n: ArithExpr,
//                               override val dt: DataType,
//                               override val out: Phrase[AccType],
//                               override val body: Phrase[ExpType -> (AccType -> CommandType)])
//      extends OpenCLParFor(n, dt, out, body) {
//      override def makeParFor = OpenCLParForSeq
//
//      override def parallelismLevel = OpenCL.Sequential
//
//      override val name: String = freshName("i_")
//
//      override lazy val init = Cst(0)
//      override lazy val step = Cst(1)
//
//      override def synchronize: Stmt = ??? //OpenCLAST.Skip()
//    }
//
////    OpenCLParForSeq(pf.n, pf.dt, pf.out, pf.body).codeGenCmd(block, env)
//    ???
//  }
//
//  // ==== generating expressions  ==== //
//
//  def toOpenCL(g: Gather,
//               env: OpenCLOldCodeGenerator.Environment,
//               arrayAccess: List[ArithExpr],
//               tupleAccess: List[ArithExpr],
//               dt: DataType): Expression = {
//    val i :: is = arrayAccess
//    val j = OperationalSemantics.evalIndexExp(g.idxF(i))
//    OpenCLOldCodeGenerator.exp(g.array, env, dt, j :: is, tupleAccess)
//  }
//
//  def toOpenCL(j: Join,
//               env: OpenCLOldCodeGenerator.Environment,
//               arrayAccess: List[ArithExpr],
//               tupleAccess: List[ArithExpr],
//               dt: DataType): Expression = {
//    val i :: is = arrayAccess
//    OpenCLOldCodeGenerator.exp(j.array, env, dt, (i / j.m) :: (i % j.m) :: is, tupleAccess)
//  }
//
//  def toOpenCL(s: Split,
//               env: OpenCLOldCodeGenerator.Environment,
//               arrayAccess: List[ArithExpr],
//               tupleAccess: List[ArithExpr],
//               dt: DataType): Expression = {
//    val i :: j :: is = arrayAccess
//    OpenCLOldCodeGenerator.exp(s.array, env, dt, ((i * s.n) + j) :: is, tupleAccess)
//  }
//
//  def toOpenCL(z: Zip,
//               env: OpenCLOldCodeGenerator.Environment,
//               arrayAccess: List[ArithExpr],
//               tupleAccess: List[ArithExpr],
//               dt: DataType): Expression = {
//    val xj :: xs = tupleAccess
//
//    if (xj == Cst(1)) {
//      return OpenCLOldCodeGenerator.exp(z.e1, env, dt, arrayAccess, xs)
//    }
//
//    if (xj == Cst(2)) {
//      return OpenCLOldCodeGenerator.exp(z.e2, env, dt, arrayAccess, xs)
//    }
//
//    throw new Exception("This should not happen")
//  }
//
//  def toOpenCL(u: Unzip,
//               env: OpenCLOldCodeGenerator.Environment,
//               arrayAccess: List[ArithExpr],
//               tupleAccess: List[ArithExpr],
//               dt: DataType): Expression = {
//    ???
//  }
//
//  def toOpenCL(f: Fst,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt: DataType,
//               arrayAccess: List[ArithExpr],
//               tupleAccess: List[ArithExpr]): Expression = {
//    OpenCLOldCodeGenerator.exp(f.record, env, dt, arrayAccess, 1 :: tupleAccess)
//  }
//
//  def toOpenCL(i: Idx,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt: DataType,
//               arrayAccess: List[ArithExpr],
//               tupleAccess: List[ArithExpr]): Expression = {
//
//    val idx: ArithExpr = OpenCLOldCodeGenerator.exp(i.index, env) match {
//      case VarRef(name, _, _) => NamedVar(name, env.ranges(name))
//      case ArithExpression(ae) => ae
//      case _ => throw new Exception("This should not happen")
//    }
//
//    OpenCLOldCodeGenerator.exp(i.array, env, dt, idx :: arrayAccess, tupleAccess)
//  }
//
//  def toOpenCL(i:DepIdx,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt:DataType,
//               arrayAccess: List[ArithExpr],
//               tupleAccess: List[ArithExpr]):Expression = {
//
//    val idx: ArithExpr = i.index
////      OpenCLOldCodeGenerator.exp(i.index, env) match {
////      case VarRef(name, _, _) => NamedVar(name, env.ranges(name))
////      case ArithExpression(ae) => ae
////      case _ => throw new Exception("This should not happen")
////    }
//
//    OpenCLOldCodeGenerator.exp(i.array, env, dt, idx :: arrayAccess, tupleAccess)
//  }
//
//
//  def toOpenCL(r: Record,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt: DataType,
//               arrayAccess: List[ArithExpr],
//               tupleAccess: List[ArithExpr]): Expression = ???
//
//  def toOpenCL(s: Snd,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt: DataType,
//               arrayAccess: List[ArithExpr],
//               tupleAccess: List[ArithExpr]): Expression = {
//    OpenCLOldCodeGenerator.exp(s.record, env, dt, arrayAccess, 2 :: tupleAccess)
//  }
//
//  def toOpenCL(t: TruncExp,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt: DataType,
//               arrayAccess: List[ArithExpr],
//               tupleAccess: List[ArithExpr]): Expression = {
//    OpenCLOldCodeGenerator.exp(t.array, env, dt, arrayAccess, tupleAccess)
//  }
//
//  def toOpenCL(l: DPIA.Phrases.Literal,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt: DataType,
//               arrayAccess: List[ArithExpr],
//               tupleAccess: List[ArithExpr]): Expression = {
//    import OperationalSemantics._
//
//    l.d match {
//      case i: IntData => OpenCLAST.Literal(i.i.toString)
//      case b: BoolData => OpenCLAST.Literal(b.b.toString)
//      case f: FloatData => OpenCLAST.Literal(f.f.toString)
//      case i: IndexData => OpenCLAST.ArithExpression(i.n)
//      case _: VectorData => ???
//      case ad: ArrayData =>
//        val nestedLiteral = DPIA.Phrases.Literal(ad.a(0))
//        OpenCLOldCodeGenerator.exp(nestedLiteral, env, nestedLiteral.d.dataType, arrayAccess.tail, tupleAccess)
//      case _: RecordData => ???
//    }
//  }
//
//  // ==== generating var refs  ==== //
//
//  def toOpenCL(r: RecordAcc1,
//               value: Expression,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt: DataType,
//               arrayAccess: List[Nat],
//               tupleAccess: List[Nat]): Expression = {
//    OpenCLOldCodeGenerator.acc(r.record, value, env, dt, arrayAccess, 1 :: tupleAccess)
//  }
//
//  def toOpenCL(i: IdxAcc,
//               value: Expression,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt: DataType,
//               arrayAccess: List[Nat],
//               tupleAccess: List[Nat]): Expression = {
//    val idx: ArithExpr = OpenCLOldCodeGenerator.exp(i.index, env) match {
//      case VarRef(name, _, _) => NamedVar(name, env.ranges(name))
//      case ArithExpression(ae) => ae
//      case _ => ???
//    }
//
//    OpenCLOldCodeGenerator.acc(i.array, value, env, dt, idx :: arrayAccess, tupleAccess)
//  }
//
//  def toOpenCL(i: DepIdxAcc,
//               value: Expression,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt: DataType,
//               arrayAccess: List[Nat],
//               tupleAccess: List[Nat]): Expression = {
//    val idx: ArithExpr = i.index
////      OpenCLOldCodeGenerator.exp(i.index, env) match {
////      case VarRef(name, _, _) => NamedVar(name, env.ranges(name))
////      case ArithExpression(ae) => ae
////      case _ => ???
////    }
//
//    OpenCLOldCodeGenerator.acc(i.array, value, env, dt, idx :: arrayAccess, tupleAccess)
//  }
//
//  def toOpenCL(join: JoinAcc,
//               value: Expression,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt: DataType,
//               arrayAccess: List[Nat],
//               tupleAccess: List[Nat]): Expression = {
//    val i :: j :: is = arrayAccess
//
//    OpenCLOldCodeGenerator.acc(join.array, value, env, dt, ((i * join.m) + j) :: is, tupleAccess)
//  }
//
//  def toOpenCL(r: RecordAcc2,
//               value: Expression,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt: DataType,
//               arrayAccess: List[Nat],
//               tupleAccess: List[Nat]): Expression = {
//    OpenCLOldCodeGenerator.acc(r.record, value, env, dt, arrayAccess, 2 :: tupleAccess)
//  }
//
//  def toOpenCL(s: SplitAcc,
//               value: Expression,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt: DataType,
//               arrayAccess: List[Nat],
//               tupleAccess: List[Nat]): Expression = {
//    val i :: is = arrayAccess
//    OpenCLOldCodeGenerator.acc(s.array, value, env, dt, (i / s.n) :: (i % s.n) :: is, tupleAccess)
//  }
//
//  def toOpenCL(t: TruncAcc,
//               value: Expression,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt: DataType,
//               arrayAccess: List[Nat],
//               tupleAccess: List[Nat]): Expression = {
//    OpenCLOldCodeGenerator.acc(t.array, value, env, dt, arrayAccess, tupleAccess)
//  }
//
//  def toOpenCL(s: ScatterAcc,
//               value: Expression,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt: DataType,
//               arrayAccess: List[Nat],
//               tupleAccess: List[Nat]): Expression = {
//    val i :: is = arrayAccess
//
//    val j = OperationalSemantics.evalIndexExp(s.idxF(i))
//
//    OpenCLOldCodeGenerator.acc(s.array, value, env, dt, j :: is, tupleAccess)
//  }
//
//  def toOpenCL(u: UnzipAcc,
//               value: Expression,
//               env: OpenCLOldCodeGenerator.Environment,
//               dt: DataType,
//               arrayAccess: List[Nat],
//               tupleAccess: List[Nat]): Expression = {
//    // TODO: FIX THIS
//    ???
//  }
//}
