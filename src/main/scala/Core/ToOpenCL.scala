package Core

import Core.OperationalSemantics._
import Core.PhraseType.x
import opencl.generator.OpenCLAST._

object ToOpenCL {

  def cmd(p: Phrase[CommandType], block: Block): Block = {
    p match {
      case IfThenElsePhrase(condP, thenP, elseP) =>
        val trueBlock = cmd(thenP, Block())
        val falseBlock = cmd(elseP, Block())
        (block: Block) += IfThenElse(exp(condP), trueBlock, falseBlock)

      case c: CommandPattern => c.toOpenCL(block)

      case ApplyPhrase(_, _) | IdentPhrase(_) | Proj1Phrase(_) | Proj2Phrase(_) => throw new Exception("This should not happen")
    }
  }

  def exp(p: Phrase[ExpType]): Expression = {
    p match {
      case ApplyPhrase(fun, arg) => ???
      case BinOpPhrase(op, lhs, rhs) =>
        exp(rhs)
      case IdentPhrase(name) => ???
      case IfThenElsePhrase(condP, thenP, elseP) => ???
      case LiteralPhrase(d) =>
        d match {
          case i: IntData => Literal(i.i.toString)
          case b: BoolData => Literal(b.b.toString)
          case f: FloatData => Literal(f.f.toString)
          case i: IndexData => Literal(i.i.toString)
          case i: Int4Data => ???
          case _: RecordData => ???
          case _: ArrayData => ???
        }
      case p: Proj1Phrase[ExpType, _] =>
        p.pair match {
          case i: IdentPhrase[ExpType x AccType] => VarRef(i.name)
          case _ => ???
        }
      case Proj2Phrase(pair) => ???
      case UnaryOpPhrase(op, x) => ???
      case e: ExpPattern => e.toOpenCL
    }
  }

  def acc(p: Phrase[AccType]): OclAstNode = {
    p match {
      case ApplyPhrase(fun, arg) => ???
      case IdentPhrase(name) => ???
      case IfThenElsePhrase(condP, thenP, elseP) => ???
      case Proj1Phrase(pair) => ???
      case p: Proj2Phrase[_, AccType] =>
        p.pair match {
          case i: IdentPhrase[ExpType x AccType] => VarRef(i.name)
          case _ => ???
        }
      case a: AccPattern => a.toOpenCL
    }
  }

}
