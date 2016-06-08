package Core

import Core.OperationalSemantics._
import opencl.generator.OpenCLAST._

object ToOpenCL {

  def cmd(p: Phrase[CommandType], block: Block): Block = {
    p match {
      case IfThenElsePhrase(condP, thenP, elseP) =>
        val trueBlock = cmd(thenP, Block())
        val falseBlock = cmd(elseP, Block())
        (block: Block) += IfThenElse(exp(condP), trueBlock, falseBlock)

      case c: CommandPattern => c.toOpenCL(block)

      case ApplyPhrase(_, _) | IdentPhrase(_) | Proj1Phrase(_) | Proj2Phrase(_) =>
        throw new Exception("This should not happen")
    }
  }

  def exp(p: Phrase[ExpType]): Expression = {
    p match {
      case BinOpPhrase(op, lhs, rhs) =>
        BinaryExpression(op.toString, exp(lhs), exp(rhs))
      case IdentPhrase(name) => VarRef(name)
      case LiteralPhrase(d) =>
        d match {
          case i: IntData => Literal(i.i.toString)
          case b: BoolData => Literal(b.b.toString)
          case f: FloatData => Literal(f.f.toString)
          case i: IndexData => Literal(i.i.toString)
          case i: Int4Data => Literal(s"(int4)(${i.i0.toString}, ${i.i1.toString}, ${i.i2.toString}, ${i.i3.toString})")
          case _: RecordData => ???
          case _: ArrayData => ???
        }
      case p: Proj1Phrase[ExpType, _] => exp(Lift.liftPair(p.pair)._1)
      case p: Proj2Phrase[_, ExpType] => exp(Lift.liftPair(p.pair)._2)
      case UnaryOpPhrase(op, x) =>
        UnaryExpression(op.toString, exp(x))
      case e: ExpPattern => e.toOpenCL

      case ApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) =>
        throw new Exception("This should not happen")
    }
  }

  def acc(p: Phrase[AccType]): VarRef = {
    p match {
      case IdentPhrase(name) => VarRef(name)
      case p: Proj1Phrase[AccType, _] => acc(Lift.liftPair(p.pair)._1)
      case p: Proj2Phrase[_, AccType] => acc(Lift.liftPair(p.pair)._2)
      case a: AccPattern => a.toOpenCL

      case ApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) =>
        throw new Exception("This should not happen")
    }
  }

}
