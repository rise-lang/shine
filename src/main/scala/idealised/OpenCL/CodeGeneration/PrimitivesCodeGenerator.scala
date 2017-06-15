package idealised.OpenCL.CodeGeneration

import idealised.DPIA.DSL._
import idealised.DPIA.FunctionalPrimitives._
import idealised.DPIA.ImperativePrimitives._
import idealised.DPIA.Phrases.{Identifier, Lambda, Phrase}
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.IndexData
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.CodeGenerator
import idealised.OpenCL.ImperativePrimitives.OpenCLParFor
import idealised._
import ir.Type
import ir.ast.ArrayAccess
import lift.arithmetic._
import opencl.generator.OpenCLAST
import opencl.generator.OpenCLAST._
import opencl.ir.PtrType

object PrimitivesCodeGenerator {

  // ==== generating blocks  ==== //

  def toOpenCL(a: Assign, block: Block, env: CodeGenerator.Environment): Block = {
    (block: Block) += CodeGenerator.acc(a.lhs, CodeGenerator.exp(a.rhs, env), env)
      // AssignmentExpression(ToOpenCL.acc(a.lhs, env), ToOpenCL.exp(a.rhs, env))
  }

  def toOpenCL(f: For, block: Block, env: CodeGenerator.Environment): Block = {
    import opencl.generator.OpenCLAST._

    val name = freshName("i_")

    val updatedEnv = env.updatedRanges(name, RangeAdd(0, f.n, 1))

    val init = VarDecl(name, opencl.ir.Int,
      init = ArithExpression(0),
      addressSpace = opencl.ir.PrivateMemory)

    val cond = CondExpression(VarRef(name),
      ArithExpression(f.n),
      CondExpression.Operator.<)

    val v = NamedVar(name)
    val increment = AssignmentExpression(ArithExpression(v), ArithExpression(v + 1))

    val bodyE = Lifting.liftFunction(f.body)
    val i = identifier(name, ExpType(IndexType(f.n)))

    f.n match {
      case Cst(0) =>
        (block: Block) +=
          OpenCLAST.Comment("iteration count is 0, no loop emitted")

      case Cst(1) =>
        (block: Block) +=
          OpenCLAST.Comment("iteration count is exactly 1, no loop emitted")
        (block: Block) += CodeGenerator.cmd(bodyE(i), Block(Vector(init)), updatedEnv)

      case _ =>
        val body_ = CodeGenerator.cmd(bodyE(i), Block(), updatedEnv)
        (block: Block) += ForLoop(init, cond, increment, body_)
    }

    block
  }

  def toOpenCL(d: DoubleBufferFor, block: Block, env: CodeGenerator.Environment): Block = {
    import opencl.generator.OpenCLAST._

    val oclAddressSpace = d.addressSpace.asInstanceOf[idealised.OpenCL.AddressSpace]

    val ptrType =
      new PtrType(DataType.scalarType(d.dt), OpenCL.AddressSpace.toOpenCL(oclAddressSpace))

    // in* = buffer1
    val buffer1Name = d.buffer1 match {
      case i: Identifier[VarType] => i.name
      case _ => throw new Exception("This should not happen")
    }
    val in = identifier(freshName("in_"), ExpType(ArrayType(d.n, d.dt)))

    (block: Block) += VarDecl(in.name, ptrType,
      init = VarRef(buffer1Name), addressSpace = opencl.ir.PrivateMemory)

    // out* = buffer2
    val buffer2Name = d.buffer2 match {
      case i: Identifier[VarType] => i.name
      case _ => throw new Exception("This should not happen")
    }
    val out = identifier(freshName("out_"), AccType(ArrayType(d.n, d.dt)))

    (block: Block) += VarDecl(out.name, ptrType,
      init = VarRef(buffer2Name), addressSpace = opencl.ir.PrivateMemory)

    // for ...
    val loopVar = NamedVar(freshName("i_"))
    val updatedEnv = env.updatedRanges(loopVar.name, RangeAdd(0, d.k, 1))

    val init = VarDecl(loopVar.name, opencl.ir.Int,
      init = ArithExpression(0),
      addressSpace = opencl.ir.PrivateMemory)

    val cond = CondExpression(VarRef(loopVar.name),
      ArithExpression(d.k),
      CondExpression.Operator.<)

    val increment = AssignmentExpression(
      ArithExpression(loopVar), ArithExpression(loopVar + 1))

    val bodyE = Lifting.liftNatDependentFunction(d.body)
    val bodyEE = Lifting.liftFunction(bodyE(loopVar))
    val bodyEEE = Lifting.liftFunction(bodyEE(out))

    val nestedBlock = Block()
    val body_ = CodeGenerator.cmd(bodyEEE(in), nestedBlock, updatedEnv)

    (block: Block) += ForLoop(init, cond, increment, body_)

    val tmp = NamedVar(freshName("tmp_"))
    // tmp = in
    nestedBlock += VarDecl(tmp.name, ptrType,
      init = VarRef(in.name), addressSpace = opencl.ir.PrivateMemory)

    // in = out
    nestedBlock +=
      AssignmentExpression(
        ArithExpression(NamedVar(in.name)),
        ArithExpression(NamedVar(out.name)))

    // out = tmp
    nestedBlock +=
      AssignmentExpression(
        ArithExpression(NamedVar(out.name)),
        ArithExpression(NamedVar(tmp.name)))

    // copy result to output
    val CE = Lifting.liftFunction(d.C)(identifier(in.name, ExpType(ArrayType(d.m, d.dt))))
    TypeChecker(CE)
    (block: Block) += CodeGenerator.cmd(CE, Block(), updatedEnv)

    block
  }

  def toOpenCL(n: New, block: Block, env: CodeGenerator.Environment): Block = {
    val v = NamedVar(n.f match {
      case Lambda(x, _) => x.name
      case _ => ???
    })

    if (n.addressSpace == OpenCL.PrivateMemory) {
      val unrolledNames = unrollName(v.name, n.dt, collection.Seq())
      val unrolldTypes = unrollType(n.dt, collection.Seq())
      (unrolledNames zip unrolldTypes).foreach {
        case (name, dt) => (block: Block) += VarDecl(name, DataType.toType(dt))
      }
//      allocationInPrivateMemory(n.dt, v.name, block)
    } else {
      // TODO: throw exception
      (block: Block) += Comment(s"new ${v.name} ${n.dt} ${n.addressSpace}")
    }

    val f_ = Lifting.liftFunction(n.f)
    val v_ = identifier(v.name, n.f.t.inT)
    CodeGenerator.cmd(f_(v_), block, env)
  }

//  def allocationInPrivateMemory(dt: DataType, name: String, block: Block): Block = {
//    dt match {
//      case _: BasicType =>
//        (block: Block) += VarDecl(name, DataType.toType(dt))
//      case ct: ComposedType => ct match {
//        case ArrayType(size, et) =>
//          // array in private memory is unrolled ...
//          for (i <- 0 until size.evalInt) {
//            allocationInPrivateMemory(et, s"${name}_$i", block)
//          }
//          block
//        case rt: RecordType => ???
//      }
//      case _: DataTypeIdentifier => ???
//    }
//  }

  def unrollName(name: String, dt: DataType, seq: collection.Seq[String]): collection.Seq[String] = {
    dt match {
      case _: BasicType => seq :+ name
      case ct: ComposedType => ct match {
        case ArrayType(size, et) =>
          (0 until size.evalInt).foldLeft(seq){
            (names, i) => unrollName(s"${name}_$i", et, names)
          }
        case rt: RecordType => ???
      }
      case _: DataTypeIdentifier => ???
    }
  }

  def unrollType(dt: DataType, seq: collection.Seq[DataType]): collection.Seq[DataType] = {
    dt match {
      case _: BasicType => seq :+ dt
      case ct: ComposedType => ct match {
        case ArrayType(size, et) =>
          (0 until size.evalInt).foldLeft(seq){
            (types, _) => unrollType(et, types)
          }
        case rt: RecordType => ???
      }
      case _: DataTypeIdentifier => ???
    }
  }

  def toOpenCL(s: idealised.DPIA.ImperativePrimitives.Seq,
               block: Block,
               env: CodeGenerator.Environment): Block = {
    CodeGenerator.cmd(s.c1, block, env)
    CodeGenerator.cmd(s.c2, block, env)
  }

  def toOpenCL(s: idealised.DPIA.ImperativePrimitives.Skip,
               block: Block,
               env: CodeGenerator.Environment): Block = {
    (block: Block) += OpenCLAST.Comment("skip")
  }

  def toOpenCL(pf: ParFor,
               block: Block,
               env: CodeGenerator.Environment): Block = {

    // TODO: this is not really a par for if it is done sequentially ...
    case class OpenCLParForSeq(override val n: ArithExpr,
                               override val dt: DataType,
                               override val out: Phrase[AccType],
                               override val body: Phrase[ExpType -> (AccType -> CommandType)])
      extends OpenCLParFor(n, dt, out, body) {
      override def makeParFor = OpenCLParForSeq

      override def parallelismLevel = OpenCL.Sequential

      override val name: String = freshName("i_")

      override lazy val init = Cst(0)
      override lazy val step = Cst(1)

      override def synchronize: OclAstNode with BlockMember = OpenCLAST.Skip()
    }

    OpenCLParForSeq(pf.n, pf.dt, pf.out, pf.body).codeGenCmd(block, env)
  }

  // ==== generating expressions  ==== //

  def toOpenCL(g: Gather,
               env: CodeGenerator.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expression = {

    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val n_ = OperationalSemantics.evalIndexExp(g.idxF(idx._1))

    CodeGenerator.exp(g.array, env, dt, (n_, idx._2) :: stack, tupleAccess)

  }

  def toOpenCL(j: Join,
               env: CodeGenerator.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expression = {
    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val chunkId = idx._1 /^ j.n
    val chunkElemId = idx._1 % j.n

    val l = Type.getLengths(DataType.toType(j.t.dataType)).reduce(_ * _)

    val newAs = (chunkId, l * j.n) :: (chunkElemId, l) :: stack

    CodeGenerator.exp(j.array, env, dt, newAs, tupleAccess)
  }

  def toOpenCL(s: Split,
               env: CodeGenerator.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expression = {

    val (firstTwo, rest) = arrayAccess.splitAt(2)

    val chunkId = firstTwo.head
    val chunkElemId = firstTwo.tail.head

    val newIdx = chunkId._1 * s.n + chunkElemId._1

    CodeGenerator.exp(s.array, env, dt, (newIdx, chunkElemId._2) :: rest, tupleAccess)
  }

  def toOpenCL(z: Zip,
               env: CodeGenerator.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expression = {
    val i = tupleAccess.head
    val rest = tupleAccess.tail

    if (i == Cst(1)) {
      return CodeGenerator.exp(z.e1, env, dt, arrayAccess, rest)
    }

    if (i == Cst(2)) {
      return CodeGenerator.exp(z.e2, env, dt, arrayAccess, rest)
    }

    throw new Exception("This should not happen")
  }

  def toOpenCL(f: Fst, env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr]): Expression = {
    CodeGenerator.exp(f.record, env, dt, arrayAccess, 1 :: tupleAccess)
  }

  def toOpenCL(i: Idx, env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr]): Expression = {
    val idx: ArithExpr = CodeGenerator.exp(i.index, env) match {
      case VarRef(name, _, _) => NamedVar(name, env.ranges(name))
      case _ => throw new Exception("This should not happen")
    }
    val length = DataType.getLengths(i.dt, tupleAccess, List()).foldLeft(1: ArithExpr)((x, y) => x * y)

    CodeGenerator.exp(i.array, env, dt, (idx, length) :: arrayAccess, tupleAccess)
  }

  def toOpenCL(r: Record,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr]): Expression = ???

  def toOpenCL(s: Snd,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr]): Expression = {
    CodeGenerator.exp(s.record, env, dt, arrayAccess, 2 :: tupleAccess)
  }

  def toOpenCL(t: TruncExp,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr]): Expression = {
    CodeGenerator.exp(t.array, env, dt, arrayAccess, tupleAccess)
  }

  def toOpenCL(l: DPIA.Phrases.Literal,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr]): Expression = {
    import OperationalSemantics._

    l.dt match {
      case _: BasicType =>
        l.d match {
          case i: IntData => OpenCLAST.Literal(i.i.toString)
          case b: BoolData => OpenCLAST.Literal(b.b.toString)
          case f: FloatData => OpenCLAST.Literal(f.f.toString)
          case i: IndexData => OpenCLAST.Literal(i.i.toString)
        }
      case ct: ComposedType => (ct, l.d) match {
        case (at: ArrayType, ad: ArrayData) =>
          val nestedLiteral = DPIA.Phrases.Literal(ad.a(0), at.elemType)
          CodeGenerator.exp(nestedLiteral, env, at.elemType, arrayAccess.tail, tupleAccess)
        case (rt: RecordType, rd: RecordData) =>
          ???
      }
    }
  }

  // ==== generating var refs  ==== //

  def toOpenCL(f: RecordAcc1,
               value: Expression,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[(Nat, Nat)],
               tupleAccess: List[Nat]): Expression = ???

  def toOpenCL(i: IdxAcc,
               value: Expression,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[(Nat, Nat)],
               tupleAccess: List[Nat]): Expression = {
    val idx: ArithExpr = CodeGenerator.exp(i.index, env) match {
      case VarRef(name, _, _) => NamedVar(name, env.ranges(name))
      case OpenCLAST.Literal(j) => Cst(j.toInt)
      case _ => throw new Exception("This should not happen")
    }

    val length = DataType.getLengths(i.dt, tupleAccess, List()).foldLeft(1: ArithExpr)((x, y) => x * y)

    CodeGenerator.acc(i.array, value, env, dt, (idx, length) :: arrayAccess, tupleAccess)
  }

  def toOpenCL(j: JoinAcc,
               value: Expression,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[(Nat, Nat)],
               tupleAccess: List[Nat]): Expression = {
    val (firstTwo, rest) = arrayAccess.splitAt(2)

    val chunkId = firstTwo.head
    val chunkElemId = firstTwo.tail.head

    val newIdx = chunkId._1 * j.m + chunkElemId._1

    CodeGenerator.acc(j.array, value, env, dt, (newIdx, chunkElemId._2) :: rest, tupleAccess)
  }

  def toOpenCL(s: RecordAcc2,
               value: Expression,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[(Nat, Nat)],
               tupleAccess: List[Nat]): Expression = ???

  def toOpenCL(s: SplitAcc,
               value: Expression,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[(Nat, Nat)],
               tupleAccess: List[Nat]): Expression = {
    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val chunkId = idx._1 / s.n
    val chunkElemId = idx._1 % s.n

    val l = Type.getLengths(DataType.toType(s.t.dataType)).reduce(_ * _)

    val newAs = (chunkId, l * s.n) ::(chunkElemId, l) :: stack

    CodeGenerator.acc(s.array, value, env, dt, newAs, tupleAccess)
  }

  def toOpenCL(t: TruncAcc,
               value: Expression,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[(Nat, Nat)],
               tupleAccess: List[Nat]): Expression = {
    CodeGenerator.acc(t.array, value, env, dt, arrayAccess, tupleAccess)
  }

  def toOpenCL(s: ScatterAcc,
               value: Expression,
               env: CodeGenerator.Environment,
               dt: DataType,
               arrayAccess: List[(Nat, Nat)],
               tupleAccess: List[Nat]): Expression = {
    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val n_ = OperationalSemantics.evalIndexExp(s.idxF(idx._1))

    CodeGenerator.acc(s.array, value, env, dt, (n_, idx._2) :: stack, tupleAccess)
  }

}
