package CommandPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType._
import Compiling.SubstituteImplementations
import apart.arithmetic.{ArithExpr, NamedVar, RangeAdd}
import opencl.generator.OpenCLAST.Block
import DSL._
import opencl.ir.PtrType

import scala.xml.Elem

case class DoubleBufferFor(n: ArithExpr,
                           dt: DataType,
                           addressSpace: AddressSpace,
                           buffer1: Phrase[VarType],
                           buffer2: Phrase[VarType],
                           k: ArithExpr,
                           body: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
                           C: Phrase[ExpType -> CommandType])
  extends CommandPattern {

  override def toOpenCL(block: Block, ocl: ToOpenCL): Block = {
    import opencl.generator.OpenCLAST._

    val ptrType =
      new PtrType(DataType.scalarType(dt), AddressSpace.toOpenCL(addressSpace))

    // in* = buffer1
    val buffer1Name = buffer1 match {
      case i: IdentPhrase[VarType] => i.name
      case _ => throw new Exception("This should not happen")
    }
    val in = identifier(newName(), ExpType(ArrayType(n, dt)))

    (block: Block) += VarDecl(in.name, ptrType,
      init = VarRef(buffer1Name), addressSpace = opencl.ir.PrivateMemory)

    // out* = buffer2
    val buffer2Name = buffer2 match {
      case i: IdentPhrase[VarType] => i.name
      case _ => throw new Exception("This should not happen")
    }
    val out = identifier(newName(), AccType(ArrayType(n, dt)))

    (block: Block) += VarDecl(out.name, ptrType,
      init = VarRef(buffer2Name), addressSpace = opencl.ir.PrivateMemory)

    // for ...
    val loopVar = NamedVar(newName())
    ocl.env(loopVar.name) = RangeAdd(0, k, 1)

    val init = VarDecl(loopVar.name, opencl.ir.Int,
      init = ArithExpression(0),
      addressSpace = opencl.ir.PrivateMemory)

    val cond = CondExpression(VarRef(loopVar.name),
      ArithExpression(k),
      CondExpression.Operator.<)

    val increment = AssignmentExpression(
      ArithExpression(loopVar), ArithExpression(loopVar + 1))

    val bodyE = Lift.liftNatDependentFunction(body)
    val bodyEE = Lift.liftFunction(bodyE(loopVar))
    val bodyEEE = Lift.liftFunction(bodyEE(out))

    val nestedBlock = Block()
    val body_ = ToOpenCL.cmd(bodyEEE(in), nestedBlock, ocl)

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
    val CE = Lift.liftFunction(C)(in)
    TypeChecker(CE)
    (block: Block) += ToOpenCL.cmd(CE, Block(), ocl)

    ocl.env.remove(loopVar.name)

    block
  }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    DoubleBufferFor(fun(n), fun(dt),
      addressSpace,
      VisitAndRebuild(buffer1, fun),
      VisitAndRebuild(buffer2, fun),
      fun(k),
      VisitAndRebuild(body, fun),
      VisitAndRebuild(C, fun))
  }

  override def typeCheck(): CommandType = {
    CommandType()
  }

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    DoubleBufferFor(n, dt,
      addressSpace,
      buffer1,
      buffer2,
      k,
      SubstituteImplementations.applyNatDependentBinaryFun(body, env),
      SubstituteImplementations.applyFun(C, env))
  }

  override def prettyPrint: String = s"doubleBufferFor $buffer1 $buffer2 $k $body $C"

  override def xmlPrinter: Elem = {
    val l = body match {
      case NatDependentLambdaPhrase(l, _) => l
      case _ => throw new Exception("This should not happen")
    }
    <doubleBufferFor k={ToString(k)} n={ToString(n)} dt={ToString(dt)} addressSpace={ToString(addressSpace)}>
      <buffer1 type={ToString(VarType(ArrayType(n, dt)))}>
        {Core.xmlPrinter(buffer1)}
      </buffer1>
      <buffer2 type={ToString(VarType(ArrayType(n, dt)))}>
        {Core.xmlPrinter(buffer2)}
      </buffer2>
      <body type={ToString(l -> ((AccType(ArrayType(n, dt)) x ExpType(ArrayType(n, dt))) -> CommandType()))}>
        {Core.xmlPrinter(body)}
      </body>
      <continuation type={ToString(ExpType(ArrayType(n, dt)) -> CommandType())}>
        {Core.xmlPrinter(C)}
      </continuation>
    </doubleBufferFor>
  }
}
