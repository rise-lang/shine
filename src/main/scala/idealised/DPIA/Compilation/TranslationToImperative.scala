package idealised.DPIA.Compilation

import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.language.reflectiveCalls

object TranslationToImperative {
  def apply(p: Phrase[ExpType])
           (implicit context: TranslationContext): Phrase[CommandType] = {
    val outT = p.t
    val out = identifier("output", AccType(outT.dataType))
    acc(p)(out)
  }

  def acc(E: Phrase[ExpType])
         (A: Phrase[AccType])
         (implicit context: TranslationContext): Phrase[CommandType] = {
    E match {
      case x: Identifier[ExpType] => A :=|x.t.dataType| x

      case c: Literal => A :=|c.t.dataType| c

      case n: Natural => A :=|n.t.dataType| n

      case u@UnaryOp(op, e) =>
        con(e)(λ(u.t)(x =>
          A :=|u.t.dataType| UnaryOp(op, x)
        ))

      case b@BinOp(op, e1, e2) =>
        con(e1)(λ(b.t)(x =>
          con(e2)(λ(b.t)(y =>
            A :=|b.t.dataType| BinOp(op, x, y)
          ))
        ))

      case ep: ExpPrimitive => ep.acceptorTranslation(A)

      // on the fly beta-reduction
      case Apply(fun, arg) => acc(Lifting.liftFunction(fun).reducing(arg))(A)
      case NatDependentApply(fun, arg) => acc(Lifting.liftNatDependentFunction(fun)(arg))(A)
      case TypeDependentApply(fun, arg) => acc(Lifting.liftTypeDependentFunction(fun)(arg))(A)

      case IfThenElse(cond, thenP, elseP) =>
        con(cond)(λ(cond.t) { x =>
          `if` (x) `then` acc(thenP)(A) `else` acc(elseP)(A)
        })

      case Proj1(_) => throw new Exception("This should never happen")
      case Proj2(_) => throw new Exception("This should never happen")
    }
  }

  def mapAcc(f: Phrase[ExpType -> ExpType], E: Phrase[ExpType])
            (A: Phrase[AccType])
            (implicit context: TranslationContext): Phrase[CommandType] = {
    E match {
      case ep: ExpPrimitive => ep.mapAcceptorTranslation(f, A)

      // on the fly beta-reduction
      case Apply(fun, arg) => mapAcc(f, Lifting.liftFunction(fun).reducing(arg))(A)
      case NatDependentApply(fun, arg) => mapAcc(f, Lifting.liftNatDependentFunction(fun)(arg))(A)
      case TypeDependentApply(fun, arg) => mapAcc(f, Lifting.liftTypeDependentFunction(fun)(arg))(A)

      case IfThenElse(cond, thenP, elseP) =>
        con(cond)(λ(cond.t) { x =>
          `if` (x) `then` mapAcc(f, thenP)(A) `else` mapAcc(f, elseP)(A)
        })

      case _ => throw new Exception("This should never happen")
    }
  }

  def con(E: Phrase[ExpType])
         (C: Phrase[ExpType -> CommandType])
         (implicit context: TranslationContext): Phrase[CommandType] = {
    E match {
      case x: Identifier[ExpType] => C(x)

      case c: Literal => C(c)

      case n: Natural => C(n)

      case u@UnaryOp(op, e) =>
        con(e)(λ(u.t)(x =>
          C(UnaryOp(op, x))
        ))

      case b@BinOp(op, e1, e2) =>
        con(e1)(λ(b.t)(x =>
          con(e2)(λ(b.t)(y =>
            C(BinOp(op, x, y))
          ))
        ))

      case ep: ExpPrimitive => ep.continuationTranslation(C)

      // on the fly beta-reduction
      case Apply(fun, arg) => con(Lifting.liftFunction(fun).reducing(arg))(C)
      case NatDependentApply(fun, arg) => con(Lifting.liftNatDependentFunction(fun)(arg))(C)
      case TypeDependentApply(fun, arg) => con(Lifting.liftTypeDependentFunction(fun)(arg))(C)

      case IfThenElse(cond, thenP, elseP) =>
        con(cond)(λ(cond.t) { x =>
          `if`(x) `then` con(thenP)(C) `else` con(elseP)(C)
        })

      case Proj1(_) => throw new Exception("This should never happen")
      case Proj2(_) => throw new Exception("This should never happen")
    }
  }

}
