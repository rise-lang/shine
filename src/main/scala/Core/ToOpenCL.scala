package Core

import Core.OperationalSemantics._
import apart.arithmetic.{ArithExpr, Cst}
import opencl.generator.OpenCLAST._

import scala.collection.immutable.List

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

  def exp(p: Phrase[ExpType], arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): Expression = {
    p match {
      case IdentPhrase(name) =>
        val i = arrayAccess.map(x => x._1 * x._2).foldLeft(0: ArithExpr)((x, y) => x + y)
        val index = if (i != Cst(0)) { i } else { null }

        val s = tupleAccess.map(x => if (x == Cst(1)) { "._1" } else if (x == Cst(2)) { "._2"} ).foldLeft("")(_+_)
        val suffix = if (s != "") { s } else { null }

        VarRef(name, suffix, ArithExpression(index))

      case p: Proj1Phrase[ExpType, _] => exp(Lift.liftPair(p.pair)._1, arrayAccess, tupleAccess)
      case p: Proj2Phrase[_, ExpType] => exp(Lift.liftPair(p.pair)._2, arrayAccess, tupleAccess)

      case e: ExpPattern => e.toOpenCL(arrayAccess, tupleAccess)

      case ApplyPhrase(_, _) | BinOpPhrase(_, _, _) | UnaryOpPhrase(_, _) | IfThenElsePhrase(_, _, _) | LiteralPhrase(_) =>
        throw new Exception("This should not happen")
    }
  }

  def acc(p: Phrase[AccType], arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef = {
    p match {
      case IdentPhrase(name) =>
        val i = arrayAccess.map(x => x._1 * x._2).foldLeft(0: ArithExpr)((x, y) => x + y)
        val index = if (i != Cst(0)) { i } else { null }

        val s = tupleAccess.map(x => if (x == Cst(1)) { "._1" } else if (x == Cst(2)) { "._2"} ).foldLeft("")(_+_)
        val suffix = if (s != "") { s } else { null }

        VarRef(name, suffix, ArithExpression(index))

      case a: AccPattern => a.toOpenCL(arrayAccess, tupleAccess)

      case p: Proj1Phrase[AccType, _] => acc(Lift.liftPair(p.pair)._1, arrayAccess, tupleAccess)
      case p: Proj2Phrase[_, AccType] => acc(Lift.liftPair(p.pair)._2, arrayAccess, tupleAccess)

      case ApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) =>
        throw new Exception("This should not happen")
    }
  }
}
