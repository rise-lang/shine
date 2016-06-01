package Rewriting

import Core._
import Core.PhraseType._
import DSL._
import CommandPatterns._

object RewriteToImperative {

  def apply(lambda: LambdaPhrase[ExpType, ExpType]): Phrase[CommandType] = {
    val p = lambda(identifier("input", lambda.param.t))
    val outT = TypeChecker(p)
    val out = identifier("output", AccType(outT.dataType))
    acc(p, out)
  }

  def acc(E: Phrase[ExpType], A: Phrase[AccType]): Phrase[CommandType] = {
    E match {
      case x: IdentPhrase[ExpType] if x.t.dataType.isBasicType =>
        A `:=` x

      case x: IdentPhrase[ExpType] if x.t.dataType.isInstanceOf[ArrayType] =>
        MapI(A,
          λ(A.t) { o => λ(x.t) { x => acc(x, o) } },
          x
        )

      case x: IdentPhrase[ExpType] if x.t.dataType.isInstanceOf[RecordType] =>
        acc(fst(x), fstAcc(A)) `;` acc(snd(x), sndAcc(A))

      case c : LiteralPhrase => A `:=` c

      case BinOpPhrase(op, e1, e2) =>
        exp(e1, λ(e1.t) { x =>
          exp(e2, λ(e2.t) { y =>
            A `:=` BinOpPhrase(op, x, y)
          })
        })

      case pattern: ExpPattern => pattern.rewriteToImperativeAcc(A)

      // on the fly beta-reduction
      case ApplyPhrase(fun, arg) => acc(Lift.liftFunction(fun)(arg), A)
    }
  }

  def exp(E: Phrase[ExpType], C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    E match {
      case x: IdentPhrase[ExpType] => C(x)

      case c : LiteralPhrase => C(c)

      case BinOpPhrase(op, e1, e2) =>
        exp(e1, λ(e1.t) { x =>
          exp(e2, λ(e2.t) { y =>
            C(BinOpPhrase(op, x, y))
          })
        })

      case pattern: ExpPattern => pattern.rewriteToImperativeExp(C)

      // on the fly beta-reduction
      case ApplyPhrase(fun, arg) => exp(Lift.liftFunction(fun)(arg), C)

      case IfThenElsePhrase(cond, thenP, elseP) => throw new Exception("This should never happen")
      case Proj1Phrase(pair) => throw new Exception("This should never happen")
      case Proj2Phrase(pair) => throw new Exception("This should never happen")
    }
  }

}
