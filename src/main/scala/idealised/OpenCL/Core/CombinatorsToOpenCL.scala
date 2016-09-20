package idealised.OpenCL.Core

import idealised._
import idealised.Core._
import idealised.DSL.typed.identifier
import idealised.HighLevelCombinators._
import idealised.LowLevelCombinators._
import idealised.OpenCL.LowLevelCombinators.OpenCLParFor
import apart.arithmetic._
import ir.Type
import opencl.generator.OpenCLAST
import opencl.generator.OpenCLAST._
import opencl.ir.PtrType

object CombinatorsToOpenCL {

  // ==== generating blocks  ==== //

  def toOpenCL(a: Assign, block: Block, env: ToOpenCL.Environment): Block = {
    (block: Block) +=
      AssignmentExpression(ToOpenCL.acc(a.lhs, env), ToOpenCL.exp(a.rhs, env))
  }

  def toOpenCL(f: For, block: Block, env: ToOpenCL.Environment): Block = {
    import opencl.generator.OpenCLAST._

    val name = newName()

    env.ranges(name) = RangeAdd(0, f.n, 1)

    val init = VarDecl(name, opencl.ir.Int,
      init = ArithExpression(0),
      addressSpace = opencl.ir.PrivateMemory)

    val cond = CondExpression(VarRef(name),
      ArithExpression(f.n),
      CondExpression.Operator.<)

    val v = NamedVar(name)
    val increment = AssignmentExpression(ArithExpression(v), ArithExpression(v + 1))

    val bodyE = Lift.liftFunction(f.body)
    val i = identifier(name, ExpType(int))

    val body_ = ToOpenCL.cmd(bodyE(i), Block(), env)

    (block: Block) += ForLoop(init, cond, increment, body_)

    env.ranges.remove(name)

    block
  }

  def toOpenCL(d: DoubleBufferFor, block: Block, env: ToOpenCL.Environment): Block = {
    import opencl.generator.OpenCLAST._

    val oclAddressSpace = d.addressSpace.asInstanceOf[idealised.OpenCL.AddressSpace]

    val ptrType =
      new PtrType(DataType.scalarType(d.dt), OpenCL.AddressSpace.toOpenCL(oclAddressSpace))

    // in* = buffer1
    val buffer1Name = d.buffer1 match {
      case i: IdentPhrase[VarType] => i.name
      case _ => throw new Exception("This should not happen")
    }
    val in = identifier(newName(), ExpType(ArrayType(d.n, d.dt)))

    (block: Block) += VarDecl(in.name, ptrType,
      init = VarRef(buffer1Name), addressSpace = opencl.ir.PrivateMemory)

    // out* = buffer2
    val buffer2Name = d.buffer2 match {
      case i: IdentPhrase[VarType] => i.name
      case _ => throw new Exception("This should not happen")
    }
    val out = identifier(newName(), AccType(ArrayType(d.n, d.dt)))

    (block: Block) += VarDecl(out.name, ptrType,
      init = VarRef(buffer2Name), addressSpace = opencl.ir.PrivateMemory)

    // for ...
    val loopVar = NamedVar(newName())
    env.ranges(loopVar.name) = RangeAdd(0, d.k, 1)

    val init = VarDecl(loopVar.name, opencl.ir.Int,
      init = ArithExpression(0),
      addressSpace = opencl.ir.PrivateMemory)

    val cond = CondExpression(VarRef(loopVar.name),
      ArithExpression(d.k),
      CondExpression.Operator.<)

    val increment = AssignmentExpression(
      ArithExpression(loopVar), ArithExpression(loopVar + 1))

    val bodyE = Lift.liftNatDependentFunction(d.body)
    val bodyEE = Lift.liftFunction(bodyE(loopVar))
    val bodyEEE = Lift.liftFunction(bodyEE(out))

    val nestedBlock = Block()
    val body_ = ToOpenCL.cmd(bodyEEE(in), nestedBlock, env)

    (block: Block) += ForLoop(init, cond, increment, body_)

    val tmp = NamedVar(newName())
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
    val CE = Lift.liftFunction(d.C)(identifier(in.name, ExpType(ArrayType(d.m, d.dt))))
    TypeChecker(CE)
    (block: Block) += ToOpenCL.cmd(CE, Block(), env)

    env.ranges.remove(loopVar.name)

    block
  }

  def toOpenCL(n: New, block: Block, env: ToOpenCL.Environment): Block = {
    val v = NamedVar(newName())

    if (n.addressSpace == OpenCL.PrivateMemory) {
      (block: Block) += VarDecl(v.name, DataType.toType(n.dt))
    } else {
      // TODO: throw exception
      (block: Block) += Comment(s"new ${v.name} ${n.dt} ${n.addressSpace}")
    }

    val f_ = Lift.liftFunction(n.f)
    val v_ = identifier(v.name, n.f.t.inT)
    ToOpenCL.cmd(f_(v_), block, env)
  }

  def toOpenCL(s: idealised.LowLevelCombinators.Seq,
               block: Block,
               env: ToOpenCL.Environment): Block = {
    ToOpenCL.cmd(s.c1, block, env)
    ToOpenCL.cmd(s.c2, block, env)
  }

  def toOpenCL(s: idealised.LowLevelCombinators.Skip,
               block: Block,
               env: ToOpenCL.Environment): Block = block

  def toOpenCL(pf: ParFor,
               block: Block,
               env: ToOpenCL.Environment): Block = {

    // TODO: this is not really a par for if it is done sequentially ...
    case class OpenCLParForSeq(override val n: ArithExpr,
                               override val dt: DataType,
                               override val out: Phrase[AccType],
                               override val body: Phrase[ExpType -> (AccType -> CommandType)])
      extends OpenCLParFor(n, dt, out, body) {
      override def makeParFor = OpenCLParForSeq

      override def parallelismLevel = OpenCL.Sequential

      override lazy val init = Cst(0)
      override lazy val step = Cst(1)

      override def synchronize: OclAstNode with BlockMember = OpenCLAST.Skip()
    }

    OpenCLParForSeq(pf.n, pf.dt, pf.out, pf.body).toOpenCL(block, env)
  }

  // ==== generating expressions  ==== //

  def toOpenCL(g: Gather,
               env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expression = {
    import idealised.DSL.typed._

    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val n_ = OperationalSemantics.evalIndexExp(g.idxF(idx._1))

    ToOpenCL.exp(g.array, env, (n_, idx._2) :: stack, tupleAccess, dt)

  }

  def toOpenCL(j: Join,
               env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expression = {
    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val chunkId = idx._1 / j.n
    val chunkElemId = idx._1 % j.n

    val l = Type.getLengths(DataType.toType(j.t.dataType)).reduce(_ * _)

    val newAs = (chunkId, l * j.n) ::(chunkElemId, l) :: stack

    ToOpenCL.exp(j.array, env, newAs, tupleAccess, dt)
  }

  def toOpenCL(s: Split,
               env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expression = {

    val (firstTwo, rest) = arrayAccess.splitAt(2)

    val chunkId = firstTwo.head
    val chunkElemId = firstTwo.tail.head

    val newIdx = chunkId._1 * s.n + chunkElemId._1

    ToOpenCL.exp(s.array, env, (newIdx, chunkElemId._2) :: rest, tupleAccess, dt)
  }

  def toOpenCL(z: Zip,
               env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expression = {
    val i = tupleAccess.head
    val rest = tupleAccess.tail

    if (i == Cst(1)) {
      return ToOpenCL.exp(z.lhs, env, arrayAccess, rest, dt)
    }

    if (i == Cst(2)) {
      return ToOpenCL.exp(z.rhs, env, arrayAccess, rest, dt)
    }

    throw new Exception("This should not happen")
  }

  def toOpenCL(f: Fst, env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expression = {
    ToOpenCL.exp(f.record, env, arrayAccess, 1 :: tupleAccess, dt)
  }

  def toOpenCL(i: Idx, env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expression = {
    val idx: ArithExpr = ToOpenCL.exp(i.index, env) match {
      case VarRef(name, _, _) => NamedVar(name, env.ranges(name))
      case _ => throw new Exception("This should not happen")
    }
    val length = DataType.getLengths(i.dt, tupleAccess, List()).foldLeft(1: ArithExpr)((x, y) => x * y)

    ToOpenCL.exp(i.array, env, (idx, length) :: arrayAccess, tupleAccess, dt)
  }

  def toOpenCL(r: Record,
               env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expression = ???

  def toOpenCL(s: Snd,
               env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expression = {
    ToOpenCL.exp(s.record, env, arrayAccess, 2 :: tupleAccess, dt)
  }

  def toOpenCL(t: TruncExp,
               env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): Expression = {
    ToOpenCL.exp(t.array, env, arrayAccess, tupleAccess, dt)
  }

  // ==== generating var refs  ==== //

  def toOpenCL(f: FstAcc,
               env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr], dt: DataType): VarRef = ???

  def toOpenCL(i: IdxAcc,
               env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): VarRef = {
    val idx: ArithExpr = ToOpenCL.exp(i.index, env) match {
      case VarRef(name, _, _) => NamedVar(name, env.ranges(name))
      case Literal(j) => Cst(j.toInt)
      case _ => throw new Exception("This should not happen")
    }
    val length = DataType.getLengths(i.dt, tupleAccess, List()).foldLeft(1: ArithExpr)((x, y) => x * y)
    ToOpenCL.acc(i.array, env, (idx, length) :: arrayAccess, tupleAccess, dt)
  }

  def toOpenCL(j: JoinAcc,
               env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): VarRef = {

    val (firstTwo, rest) = arrayAccess.splitAt(2)

    val chunkId = firstTwo.head
    val chunkElemId = firstTwo.tail.head

    val newIdx = chunkId._1 * j.m + chunkElemId._1

    ToOpenCL.acc(j.array, env, (newIdx, chunkElemId._2) :: rest, tupleAccess, dt)
  }

  def toOpenCL(r: RecordAcc,
               env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr], dt: DataType): VarRef = ???

  def toOpenCL(s: SndAcc,
               env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr], dt: DataType): VarRef = ???

  def toOpenCL(s: SplitAcc,
               env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): VarRef = {
    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val chunkId = idx._1 / s.n
    val chunkElemId = idx._1 % s.n

    val l = Type.getLengths(DataType.toType(s.t.dataType)).reduce(_ * _)

    val newAs = (chunkId, l * s.n) ::(chunkElemId, l) :: stack

    ToOpenCL.acc(s.array, env, newAs, tupleAccess, dt)
  }

  def toOpenCL(t: TruncAcc,
               env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): VarRef = {
    ToOpenCL.acc(t.array, env, arrayAccess, tupleAccess, dt)
  }

}
