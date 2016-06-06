package Core

import opencl.generator.OpenCLAST._

object ToOpenCL {
  def apply(p: Phrase[CommandType], block: Block): Unit = {
    p match {
      case ApplyPhrase(fun, arg) =>

      case IdentPhrase(name) =>

      case IfThenElsePhrase(condP, thenP, elseP) =>

      case Proj1Phrase(pair) =>

      case Proj2Phrase(pair) =>

      case c: CommandPattern =>
    }
  }

  def apply(p: Phrase[ExpType]): Unit = {
    p match {
      case ApplyPhrase(fun, arg) =>

      case BinOpPhrase(op, lhs, rhs) =>

      case IdentPhrase(name) =>

      case IfThenElsePhrase(condP, thenP, elseP) =>

      case LiteralPhrase(d) =>

      case Proj1Phrase(pair) =>

      case Proj2Phrase(pair) =>

      case UnaryOpPhrase(op, x) =>

      case e: ExpPattern =>
    }
  }
}
