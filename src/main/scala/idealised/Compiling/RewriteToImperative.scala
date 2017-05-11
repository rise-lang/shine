package idealised.Compiling

import idealised.Core._
import idealised.DSL.typed._
import idealised.IntermediatePrimitives.MapI

object RewriteToImperative {

  def apply(lambda: Lambda[ExpType, ExpType]): Phrase[CommandType] = {
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
      case x: Identifier[ExpType] =>
        x.t.dataType match {
          case _: BasicType => A `:=` x
          case ArrayType(n, dt) => MapI(n, dt, dt, A, λ(AccType(dt))(o => λ(ExpType(dt))(x => acc(x)(o))), x)
          case RecordType(dt1, dt2) => acc(fst(x))(recordAcc1(dt1, dt2, A)) `;` acc(snd(x))(recordAcc2(dt1, dt2, A))
          case _: DataTypeIdentifier => throw new Exception("This should not happen")
        }

      case c: Literal => A `:=` c

      case u@UnaryOp(op, e) =>
        exp(e)(λ(u.t)(x =>
          A `:=` UnaryOp(op, e)
        ))

      case b@BinOp(op, e1, e2) =>
        exp(e1)(λ(b.t)(x =>
          exp(e2)(λ(b.t)(y =>
            A `:=` BinOp(op, x, y)
          ))
        ))

      case ep: ExpPrimitive => ep.rewriteToImperativeAcc(A)

      // on the fly beta-reduction
      case Apply(fun, arg) => acc(Lifting.liftFunction(fun)(arg))(A)
      case NatDependentApply(fun, arg) => acc(Lifting.liftNatDependentFunction(fun)(arg))(A)
      case TypeDependentApply(fun, arg) => acc(Lifting.liftTypeDependentFunction(fun)(arg))(A)

      case IfThenElse(cond, thenP, elseP) =>
        exp(cond)(λ(cond.t) { x =>
          `if`(x, acc(thenP)(A), acc(elseP)(A))
        })

      case Proj1(pair) => throw new Exception("This should never happen")
      case Proj2(pair) => throw new Exception("This should never happen")
    }
  }

  def exp(E: Phrase[ExpType])(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    E match {
      case x: Identifier[ExpType] => C(x)

      case c: Literal => C(c)

      case u@UnaryOp(op, e) =>
        exp(e)(λ(u.t)(x =>
          C(UnaryOp(op, x))
        ))

      case b@BinOp(op, e1, e2) =>
        exp(e1)(λ(b.t)(x =>
          exp(e2)(λ(b.t)(y =>
            C(BinOp(op, x, y))
          ))
        ))

      case ep: ExpPrimitive => ep.rewriteToImperativeExp(C)

      // on the fly beta-reduction
      case Apply(fun, arg) => exp(Lifting.liftFunction(fun)(arg))(C)
      case NatDependentApply(fun, arg) => exp(Lifting.liftNatDependentFunction(fun)(arg))(C)
      case TypeDependentApply(fun, arg) => exp(Lifting.liftTypeDependentFunction(fun)(arg))(C)

      case IfThenElse(cond, thenP, elseP) =>
        exp(cond)(λ(cond.t) { x =>
          `if`(x, exp(thenP)(C), exp(elseP)(C))
        })

      case Proj1(pair) => throw new Exception("This should never happen")
      case Proj2(pair) => throw new Exception("This should never happen")
    }
  }

}
