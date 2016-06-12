package CommandPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType._
import DSL.identifier
import Rewriting.SubstituteImplementations
import apart.arithmetic.{NamedVar, Var}
import opencl.generator.OpenCLAST.{Block, VarDecl}

case class New(dt: DataType, addressSpace: AddressSpace, f: Phrase[(ExpType x AccType) -> CommandType]) extends CommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    TypeChecker(f) match {
      case FunctionType(PairType(ExpType(d1), AccType(d2)), CommandType()) =>
        if (dt == d1 && d1 == d2) {
          CommandType()
        } else {
          error(dt.toString + ", " + d1.toString + ", and " + d2.toString, expected = "them to match")
        }
      case x => error(x.toString, FunctionType.toString + "(" + PairType.toString +
        "(" + ExpType.toString + "(A)," + AccType.toString + "(A))," + CommandType() + ")")
    }
  }

  override def eval(s: Store): Store = {
    val f_ = OperationalSemantics.eval(s, f)
    val arg = identifier.newVar(newName())
    val newStore = OperationalSemantics.eval(s + (arg.name -> 0), f_(arg))
    newStore - arg.name
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): CommandPattern = {
    New(dt, addressSpace, OperationalSemantics.substitute(phrase, `for`, f))
  }

  override def substituteImpl: Phrase[CommandType] = New(dt, addressSpace, SubstituteImplementations.applyFun(f))

  override def toOpenCL(block: Block): Block = {
    val v = NamedVar(newName())

    if (addressSpace == PrivateMemory) {
      (block: Block) += VarDecl(v.name, DataType.toType(dt))
    } else {
      // TODO: allocate elsewhere
    }

    val f_ = Lift.liftFunction(f)
    val v_ = identifier.newVar(v.name, dt)
    ToOpenCL.cmd(f_(v_), block)
  }

  override def prettyPrint: String = s"new $dt $addressSpace ${PrettyPrinter(f)}"

}
