package Core

import scala.language.postfixOps
import scala.language.reflectiveCalls

object TypeOf {
  def apply[T <: PhraseType](phrase: Phrase[T]): T = {
    (phrase match {
      case x: IdentPhrase[T] => x.t
      case LambdaPhrase(x, p) => x.t -> p.t
      case ApplyPhrase(p, q) =>
        p.t match {
          case FunctionType(_, t2) => t2
        }

      case NatDependentLambdaPhrase(a, p) => a -> p.t

      case NatDependentApplyPhrase(p, e) =>
        p.t match {
          case NatDependentFunctionType(a, t) =>
            t `[` e `/` a `]`
        }

      case PairPhrase(p, q) => p.t x q.t

      case Proj1Phrase(p) =>
        p.t match {
          case PairType(t1, _) => t1
        }

      case Proj2Phrase(p) =>
        p.t match {
          case PairType(_, t2) => t2
        }

      case IfThenElsePhrase(cond, thenP, elseP) =>
        thenP.t

      case LiteralPhrase(d) => ExpType(d.dataType)

      case UnaryOpPhrase(op, x) =>
        x.t match {
          case ExpType(dt) => x.t
        }

      case BinOpPhrase(op, lhs, rhs) =>
        op match {
          case BinOpPhrase.Op.GT | BinOpPhrase.Op.LT => exp"[$bool]"
          case _ => lhs.t
        }

      case p: ExpPattern => p.t

      case p: AccPattern => p.t

      case p: IntermediateCommandPattern => p.t
    }).asInstanceOf[T]
  }
}
