package Compiling

import Core._
import DSL.typed._
import MidLevelCombinators.MapI

object RewriteToImperative {

  def apply(lambda: LambdaPhrase[ExpType, ExpType]): Phrase[CommandType] = {
    val p: Phrase[ExpType] = lambda(identifier("input", lambda.param.t))
    val outT = p.t
    val out = identifier("output", AccType(outT.dataType))
    acc(p)(out)
  }

  def apply(p: Phrase[ExpType]): Phrase[CommandType] = {
    val outT = p.t
    val out = identifier("output", AccType(outT.dataType))
    acc(p)(out)
  }

  def acc(E: Phrase[ExpType])(A: Phrase[AccType]): Phrase[CommandType] = {
    E match {
      case x: IdentPhrase[ExpType] =>
        x.t.dataType match {
          case _: BasicType | _: VectorType => A `:=` x
          case ArrayType(n, dt) => MapI(n, dt, dt, A, λ(AccType(dt))(o => λ(ExpType(dt))(x => acc(x)(o))), x)
          case RecordType(dt1, dt2) => acc(fst(x))(fstAcc(dt1, dt2, A)) `;` acc(snd(x))(sndAcc(dt1, dt2, A))
          case _: DataTypeIdentifier => throw new Exception("This should not happen")
        }

      case c: LiteralPhrase => A `:=` c

      case u@UnaryOpPhrase(op, e) =>
        exp(e)(λ(u.t)(x =>
          A `:=` UnaryOpPhrase(op, e)
        ))

      case b@BinOpPhrase(op, e1, e2) =>
        exp(e1)(λ(b.t)(x =>
          exp(e2)(λ(b.t)(y =>
            A `:=` BinOpPhrase(op, x, y)
          ))
        ))

      case hl: HighLevelCombinator => hl.rewriteToImperativeAcc(A)
      case ll: LowLevelExpCombinator => ll.rewriteToImperativeAcc(A)

      // on the fly beta-reduction
      case ApplyPhrase(fun, arg) => acc(Lift.liftFunction(fun)(arg))(A)
      case NatDependentApplyPhrase(fun, arg) => acc(Lift.liftNatDependentFunction(fun)(arg))(A)
      case TypeDependentApplyPhrase(fun, arg) => acc(Lift.liftTypeDependentFunction(fun)(arg))(A)

      case IfThenElsePhrase(cond, thenP, elseP) =>
        exp(cond)(λ(cond.t) { x =>
          `if`(x, acc(thenP)(A), acc(elseP)(A))
        })

      case Proj1Phrase(pair) => throw new Exception("This should never happen")
      case Proj2Phrase(pair) => throw new Exception("This should never happen")
    }
  }

  def exp(E: Phrase[ExpType])(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    E match {
      case x: IdentPhrase[ExpType] => C(x)

      case c: LiteralPhrase => C(c)

      case u@UnaryOpPhrase(op, e) =>
        exp(e)(λ(u.t)(x =>
          C(UnaryOpPhrase(op, x))
        ))

      case b@BinOpPhrase(op, e1, e2) =>
        exp(e1)(λ(b.t)(x =>
          exp(e2)(λ(b.t)(y =>
            C(BinOpPhrase(op, x, y))
          ))
        ))

      case hl: HighLevelCombinator => hl.rewriteToImperativeExp(C)
      case ll: LowLevelExpCombinator => ll.rewriteToImperativeExp(C)

      // on the fly beta-reduction
      case ApplyPhrase(fun, arg) => exp(Lift.liftFunction(fun)(arg))(C)
      case NatDependentApplyPhrase(fun, arg) => exp(Lift.liftNatDependentFunction(fun)(arg))(C)
      case TypeDependentApplyPhrase(fun, arg) => exp(Lift.liftTypeDependentFunction(fun)(arg))(C)

      case IfThenElsePhrase(cond, thenP, elseP) =>
        exp(cond)(λ(cond.t) { x =>
          `if`(x, exp(thenP)(C), exp(elseP)(C))
        })

      case Proj1Phrase(pair) => throw new Exception("This should never happen")
      case Proj2Phrase(pair) => throw new Exception("This should never happen")
    }
  }

}
