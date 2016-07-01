package CommandPatterns

import Core._
import Core.OperationalSemantics._
import DSL.identifier
import Compiling.SubstituteImplementations
import apart.arithmetic.NamedVar
import opencl.generator.OpenCLAST.{Block, Comment, VarDecl}

import scala.xml.Elem

case class New(dt: DataType,
               addressSpace: AddressSpace,
               f: Phrase[(ExpType x AccType) -> CommandType])
  extends CommandPattern {

  override def typeCheck(): Unit = {
    import TypeChecker._
    f checkType t"var[$dt] -> comm"
  }

  override def eval(s: Store): Store = {
    val f_ = OperationalSemantics.eval(s, f)
    val arg = identifier.newVar(newName())
    val newStore = OperationalSemantics.eval(s + (arg.name -> 0), f_(arg))
    newStore - arg.name
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    New(fun(dt), addressSpace, VisitAndRebuild(f, fun))
  }

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    val p = f match {
      case LambdaPhrase(param, _) => param
      case _ => throw new Exception("This should not happen")
    }
    New(dt, addressSpace,
      SubstituteImplementations.applyFun(f,
        env.copy(env.addressspace.updated(p.name, addressSpace)) ))
  }

  override def toOpenCL(block: Block, env: ToOpenCL.Environment): Block = {
    val v = NamedVar(newName())

    if (addressSpace == PrivateMemory) {
      (block: Block) += VarDecl(v.name, DataType.toType(dt))
    } else {
      // TODO: allocate elsewhere
      (block: Block) += Comment(s"new ${v.name} $dt $addressSpace")
    }

    val f_ = Lift.liftFunction(f)
    val v_ = identifier.newVar(v.name, dt)
    ToOpenCL.cmd(f_(v_), block, env)
  }

  override def prettyPrint: String = s"(new $addressSpace ${PrettyPrinter(f)})"

  override def xmlPrinter: Elem =
    <new dt={ToString(dt)} addressspace={ToString(addressSpace)}>
      {Core.xmlPrinter(f)}
    </new>
}
